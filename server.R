library(shiny)
library(shinyapps)
library(tidyr)
library(dplyr)
library(ggplot2)
library(pwr)
library(magrittr)
library(metafor)
library(readr)

# input <- list(dataset = "inworddb", method = "All", sig.level = 0.05, power = 0.8,
#               moderator = c("method", "procedure"))

map_procedure <- function(procedure) {  
  switch(as.character(procedure),
         "FC" = "Forced choice",
         "CHT" = "Conditioned head-turn",
         "HAB" = "Habituation",
         "FAM" = "Familiarization", 
         "OTHER" = "Other")
}

map_method <- function(method) {
  switch(as.character(method),
         "ET" = "Eyetracking",
         "B" = "Behavioral",
         "EEG" = "EEG")  
}


shinyServer(function(input, output, session) {
  
  method <- reactive({
    ifelse(is.null(input$method), "All", input$method)
  })
  
  data <- reactive({
    read_csv(paste0('data/', input$dataset, '.csv')) %>%
      rowwise() %>%
      mutate(method = map_method(method),
             procedure = map_procedure(procedure),
             procedure = sprintf('%s (%s)', procedure, procedure_secondary),
             method_procedure = paste(method, procedure),
             d = d, #TODO: calculate effect size
             d_var = 0.5, # TODO: calculate effect size variance
             mean_age = weighted.mean(c(mean_age_1, mean_age_2), c(n_1, n_2),
                                      na.rm = TRUE),
             n = mean(c(n_1, n_2), na.rm = TRUE)) %>%
      filter(!is.na(d))
  })
  
  moderator <- reactive({input$moderator})
  
  moderator_opts <- reactive({
    Filter(function(moderator) {length(unique(data()[[moderator]])) > 1},
           list("Age" = "mean_age", "Method" = "method",
                "Procedure" = "procedure"))
  })
  
  output$moderator <- renderUI({
    selectInput("moderator", label = h4("Moderator"), choices = moderator_opts(),
                multiple = TRUE)
  })
  
  model <- reactive({
    if (is.null(moderator())) {
      rma(d, vi = d_var, slab = as.character(short_cite), data = data(), method = "REML")
    } else {
      rma(as.formula(paste("d ~", paste(moderator(), collapse = "+"))),
          vi = d_var, slab = as.character(short_cite), data = data(), method = "REML")
    }
  })
  
  effect_size <- reactive({
    model$
  })
  
  #   output$method <- renderUI({
  #     selectizeInput("method", "Method", choices = c("All", unique(data()$method)),
  #                    select = "All")
  #   })
  
  #   effect_size <- reactive({
  #     filtered <- data() %>%
  #       filter(!is.na(effect_size_weight))
  #     if (method() != "All") {
  #       filtered %<>% filter(method_name == input$method)
  #     }
  #     summarise(filtered, mean.effect = weighted.mean(effect_size, effect_size_weight))[[1]]
  #   })
  
    output$effect_size <- renderText({
      sprintf("Estimated effect size is %s.", round(effect_size(), 2))
    })
  
  #   sample_size <- reactive({
  #     pwr.t.test(d = effect_size(), sig.level = input$sig.level,
  #                power = input$power, type = c("two.sample", "one.sample", "paired"))$n
  #   })
  
  #   output$sample_size <- renderText({
  #     sprintf("Estimated sample size is %s subjects in each group.", ceiling(sample_size()))
  #   })
  
  output$scatter <- renderPlot({
    print(moderator())
    mod_group <- if (is.null(moderator())) {
      NULL
    } else if ("method" %in% moderator() & "procedure" %in% moderator()) {
      "method_procedure"
    } else if ("method" %in% moderator()) {
      "method"
    } else if ("procedure" %in% moderator()) {
      "procedure"
    }
    ggplot(data(), aes_string(x = "mean_age", y = "d", colour = mod_group)) +
      geom_point(aes(size = n)) +
      geom_smooth(method = "lm", formula = y ~ log(x)) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "grey") +
      scale_colour_brewer(name = "Procedure", palette = "Set1") +
      scale_size_continuous(name = "n") +
      xlab("\nMean Subject Age (Days)") +
      ylab("Effect Size\n") +
      theme_bw(base_size=14) +
      theme(text = element_text(family = "Open Sans"))
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
  #       theme(text = element_text(family = "Open Sans"))
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
    if (is.null(moderator())) {
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
      theme(text = element_text(family = "Open Sans"), 
            panel.background = element_rect(fill = "grey"),
            panel.grid.major =  element_line(colour = "darkgrey", size = 0.2),
            panel.grid.minor =  element_line(colour = "darkgrey", size = 0.5))
    
    
  }, height = 500)
  
})