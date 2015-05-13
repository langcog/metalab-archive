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

#input <- list(dataset_name = "Word Segmentation", moderator = c("method", "procedure"))

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

datasets <- fromJSON(txt = "../datasets.json")

shinyServer(function(input, output, session) {
  
  output$datasets <- renderUI({
    selectInput("dataset_name", label = h4("Dataset"), choices = datasets$name)
  })
  
  dataset <- reactive({
    filter(datasets, name == input$dataset_name)
  })
  
  data <- reactive({
    dataset_url <- sprintf("https://docs.google.com/spreadsheets/d/%s/export?gid=0&format=csv",
                           dataset()$key)
    read_csv(getURL(dataset_url)) %>%
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
  
  moderator_opts <- reactive({
    Filter(function(moderator) {length(unique(data()[[moderator]])) > 1},
           list("Age" = "mean_age", "Method" = "method",
                "Procedure" = "procedure"))
  })
  
  output$moderators <- renderUI({
    selectInput("moderator", label = h4("Moderator"), choices = moderator_opts(),
                multiple = TRUE)
  })
  
  model <- reactive({
    if (is.null(input$moderator)) {
      rma(d, vi = d_var, slab = as.character(short_cite), data = data(), method = "REML")
    } else {
      rma(as.formula(paste("d ~", paste(input$moderator, collapse = "+"))),
          vi = d_var, slab = as.character(short_cite), data = data(), method = "REML")
    }
  })
  
#   effect_size <- reactive({
#     model$
#   })
  
#   output$effect_size <- renderText({
#     sprintf("Estimated effect size is %s.", round(effect_size(), 2))
#   })
  
  #   sample_size <- reactive({
  #     pwr.t.test(d = effect_size(), sig.level = input$sig.level,
  #                power = input$power, type = c("two.sample", "one.sample", "paired"))$n
  #   })
  
  #   output$sample_size <- renderText({
  #     sprintf("Estimated sample size is %s subjects in each group.", ceiling(sample_size()))
  #   })
  
  output$scatter <- renderPlot({
    mod_group <- if (is.null(input$moderator)) {
      NULL
    } else if ("method" %in% input$moderator & "procedure" %in% input$moderator) {
      "method_procedure"
    } else if ("method" %in% input$moderator) {
      "method"
    } else if ("procedure" %in% input$moderator) {
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
    if (is.null(input$moderator)) {
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