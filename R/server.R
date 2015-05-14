library(shiny)
library(shinyapps)
library(tidyr)
library(dplyr)
library(ggplot2)
library(pwr)
library(magrittr)
library(metafor)
library(readr)
library(RCurl)
library(jsonlite)
font <- "Open Sans"

# input <- list(dataset_name = "Phonemic Discrimination",
#               mod_method = FALSE, mod_procedure = FALSE, mod_mean_age = FALSE,
#               method = NULL, procedure = NULL, mean_age = NULL)

map_procedure <- function(procedure) {  
  switch(as.character(procedure),
         "FC" = "Forced choice",
         "CHT" = "Conditioned head-turn",
         "HAB" = "Habituation",
         "FAM" = "Familiarization",
         "SA" = "Stimulus Alternation",
         "Oddball" = "Other",
         "AEM" = "Anticipatory Eye Movement",
         "OTHER" = "Other")
}

map_method <- function(method) {
  switch(as.character(method),
         "ET" = "Eyetracking",
         "B" = "Behavioral",
         "EEG" = "Electroencephalography",
         "PHY" = "PHY?",
         "NIRS" = "Near-infrared spectroscopy")  
}

datasets <- fromJSON(txt = "../datasets.json")

shinyServer(function(input, output, session) {
  
  output$datasets <- renderUI({
    selectInput("dataset_name", label = h4("Dataset"), choices = datasets$name)
  })
  
  dataset <- reactive({
    filter(datasets, name == input$dataset_name)
  })
  
  data <- reactive({
    dataset_url <- sprintf("https://docs.google.com/spreadsheets/d/%s/export?id=%s&format=csv",
                           dataset()$key, dataset()$key)
    read.csv(textConnection(getURL(dataset_url)), stringsAsFactors = FALSE) %>%
      rowwise() %>%
      mutate(method = map_method(method),
             procedure = map_procedure(procedure),
             procedure = if(is.na(procedure_secondary)) procedure else sprintf('%s (%s)', procedure, procedure_secondary),
             method_procedure = paste(method, procedure, sep = ": "),
             d = d, #TODO: calculate effect size
             d_var = 0.5, # TODO: calculate effect size variance
             mean_age = weighted.mean(c(mean_age_1, mean_age_2), c(n_1, n_2),
                                      na.rm = TRUE),
             n = mean(c(n_1, n_2), na.rm = TRUE)) %>%
      filter(!is.na(d))
  })
  
  moderators <- reactive({
    mod_opts <- c("method", "procedure", "mean_age")
    mod_opts[c(input$mod_method, input$mod_procedure, input$mod_mean_age)]
  })
  
  model <- reactive({
    if (length(moderators()) == 0) {
      rma(d, vi = d_var, slab = as.character(short_cite), data = data(), method = "REML")
    } else {
      rma(as.formula(paste("d ~", paste(moderators(), collapse = "+"))),
          vi = d_var, slab = as.character(short_cite), data = data(), method = "REML")
    }
  })
  
  output$method <- renderUI({
    selectInput("method", label = h5("Method"), choices = c(unique(data()$method)))
  })
  
  output$procedure <- renderUI({
    selectInput("procedure", label = h5("Procedure"), choices = c(unique(data()$procedure)))
  })
  
  output$mean_age <- renderUI({
    sliderInput("mean_age", label = h5("Mean Age (months)"),
                min = round(min(data()$mean_age)/30), max = round(max(data()$mean_age)/30),
                value = mean(round(min(data()$mean_age)/30), round(max(data()$mean_age))/30))
  })
  
  effect_size <- reactive({
    filtered_data <- data()
    if (input$mod_method) {
      filtered_data %<>% filter(method == input$method)
    }
    if (input$mod_procedure) {
      filtered_data %<>% filter(procedure == input$procedure)
    }
    if (input$mod_mean_age) {
      model <- rma(d ~ mean_age, vi = d_var, slab = as.character(short_cite),
                   data = filtered_data, method = "REML")
      predict(model, newmods = input$mean_age*30)  
    } else {
      model <- rma(d, vi = d_var, slab = as.character(short_cite),
                   data = filtered_data, method = "REML")
      predict(model)
    }
  })
  
  output$effect_size <- renderText({
    sprintf("Estimated effect size is %.2f, 95%% confidence interval (%.2f, %.2f).",
            effect_size()$pred, effect_size()$ci.lb, effect_size()$ci.ub)
  })
  
  output$power <- renderPlot({
    ns <- seq(5, 120, 5)
    es <- effect_size()$pred
    pwrs <- data.frame(
      ns = ns,
      Experimental = pwr.p.test(h = es, n = ns, sig.level = 0.05)$power,
      Control = pwr.p.test(h = 0, n = ns, sig.level = 0.05)$power#,
      #      Interaction = pwr.2p.test(h = es, n = ns, sig.level = .05)$power
    ) %>%
      gather(condition, ps, Experimental, Control) #Interaction,
    
    this.pwr <- data.frame(
      ns = rep(input$N, 2),
      ps = c(pwr.p.test(h = es, n = input$N, sig.level = 0.05)$power,
             pwr.p.test(h = 0, n = input$N, sig.level = 0.05)$power),
      condition = c("Experimental", "Control")
    )
    
    ggplot(pwrs, aes(x = ns, y = ps, colour = condition)) +
      geom_point() +
      geom_line() +
      geom_point(data = this.pwr, colour = "red", size = 6) + 
      geom_hline(yintercept = 0.8, linetype = "dashed") + 
      geom_vline(
        xintercept=pwr.p.test(h = es, sig.level = 0.05, power = 0.8)$n,
        linetype = "dashed"
      ) + 
      #        geom_vline(xintercept=pwr.2p.test(h = es, 
      #                                          sig.level = .05, 
      #                                          power = .8)$n, lty=2) +         
      ylim(c(0,1)) + 
      ylab("Power to reject the null at p < .05\n") +
      xlab("\nSample size") +
      scale_colour_brewer(name = "", palette = "Set1") +      
      theme_bw() +
      theme(text = element_text(family = font),
            legend.position = "bottom",
            legend.direction = "vertical")
  }, height = function() {
    session$clientData$output_power_width * 0.7
  })
  
  output$scatter <- renderPlot({
    mod_group <- if (length(moderators()) == 0) {
      NULL
    } else if (input$mod_method & input$mod_procedure) {
      "method_procedure"
    } else if (input$mod_method) {
      "method"
    } else if (input$mod_procedure) {
      "procedure"
    }
    ggplot(data(), aes_string(x = "mean_age", y = "d", colour = mod_group)) +
      geom_point(aes(size = n)) +
      geom_smooth(method = "lm", formula = y ~ log(x)) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "grey") +
      scale_colour_brewer(name = "", palette = "Set1") +
      scale_size_continuous(guide = FALSE) +
      xlab("\nMean Subject Age (Days)") +
      ylab("Effect Size\n") +
      theme_bw(base_size=14) +
      theme(text = element_text(family = font),
            legend.position = "bottom",
            legend.direction = "vertical")
  }, height = function() {
    session$clientData$output_scatter_width * 0.7
  })
  
  #   output$violin <- renderPlot({
  #     ggplot(data(), aes(x = factor(procedure), y = d, colour = procedure)) +
  #       geom_jitter(height = 0) +
  #       geom_violin() +
  #       geom_hline(yintercept = 0, linetype = "dotted", color = "grey") +      
  #       scale_colour_brewer(name = "", palette = "Set1") +
  #       xlab("\nMethod") +
  #       ylab("Effect Size\n") +
  #       theme_bw(base_size=14) +
  #       theme(text = element_text(family = font))
  #   })
  
  output$forest <- renderPlot({
    par(mar = c(5, 4, 0, 2))
    forest(model(),
           mlab = "Grand effect size",
           xlab = "Effect size estimate",
           ylim = c(0, model()$k + 3),
           annotate = F)
  }, height = function() {
    session$clientData$output_forest_width * 5
  })
  
  output$funnel <- renderPlot({
    if (length(moderators()) == 0) {
      d = data.frame(se = sqrt(model()$vi), es = model()$yi)
      center = mean(d$es)
      xlabel = "\nEffect Size"
    } else {
      r =  rstandard(model())
      d = data.frame(se = r$se, es = r$resid)
      center = 0
      xlabel = "\nResidual"
    }
    
    lower_lim = max(d$se) + .05*max(d$se) 
    left_lim = ifelse(center-lower_lim*1.96 < min(d$es),center-lower_lim*1.96, min(d$es) )
    right_lim = ifelse(center+lower_lim*1.96 > max(d$es),center+lower_lim*1.96, max(d$es) )
    funnel = data.frame(x=c(center-lower_lim*1.96, center, center + lower_lim*1.96), 
                        y = c(-lower_lim,0,-lower_lim))
    
    ggplot(d, aes(x = es, y = -se)) +
      scale_x_continuous(limits = c(left_lim,right_lim)) +
      scale_y_continuous(expand = c(0,0), # gets rid of extra space ggplot adds to limits
                         breaks = round(seq(0, -max(d$se), length.out = 5),2),
                         labels = round(seq(0, max(d$se), length.out = 5),2))+
      geom_polygon(data = funnel, aes(x=x,y=y),fill = "white") +
      geom_vline(xintercept = center, linetype = "dotted", color = "black", size = .5) +  
      geom_point() +
      xlab(xlabel) +
      ylab("Standard error\n") +
      theme_bw(base_size=14) +
      theme(text = element_text(family = font), 
            panel.background = element_rect(fill = "grey"),
            panel.grid.major =  element_line(colour = "darkgrey", size = 0.2),
            panel.grid.minor =  element_line(colour = "darkgrey", size = 0.5))
    
    
  }, height = 500)
  
})