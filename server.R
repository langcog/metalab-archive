library(shiny)
library(shinyapps)
library(tidyr)
library(dplyr)
library(ggplot2)
library(pwr)
library(magrittr)
library(metafor)
library(readr)

input <- list(dataset = "inworddb", method = "All", sig.level = 0.05, power = 0.8,
              moderator = NULL)

map_procedure <- function(procedure) {  
  switch(as.character(procedure),
         "FC" = "Forced choice",
         "CHT" = "Conditioned head-turn",
         "HAB" = "Habituation",
         "FAM" = "Familiarization", 
         "OTHER" = "other")
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
             method_procedure = paste(method, procedure),
             d_var = 0.5,
             mean_age = weighted.mean(c(mean_age_1, mean_age_2), c(n_1, n_2),
                                      na.rm = TRUE),
             n = mean(c(n_1, n_2), na.rm = TRUE)) %>%
      filter(!is.na(d))
  })
  
  moderator <- reactive({input$moderator})

  output$method <- renderUI({
    selectizeInput("method", "Method", choices = c("All", unique(data()$method)),
                   select = "All")
  })
  
#   effect_size <- reactive({
#     filtered <- data() %>%
#       filter(!is.na(effect_size_weight))
#     if (method() != "All") {
#       filtered %<>% filter(method_name == input$method)
#     }
#     summarise(filtered, mean.effect = weighted.mean(effect_size, effect_size_weight))[[1]]
#   })
  
#   output$effect_size <- renderText({
#     sprintf("Estimated effect size is %s.", round(effect_size(), 2))
#   })
  
#   sample_size <- reactive({
#     pwr.t.test(d = effect_size(), sig.level = input$sig.level,
#                power = input$power, type = c("two.sample", "one.sample", "paired"))$n
#   })
  
  model <- reactive({
    if (moderator() == "none") {
      rma(d, vi = d_var,
          slab = as.character(short_cite), data = data(), method = "REML")      
    } else {
      rma(d ~ eval(parse(text=moderator())), vi = d_var,
          slab = as.character(short_cite), data = data(), method = "REML")
    }
  })

#   grp_model <- reactive({
#     rma(effect_size, sei = effect_size_se, mods = ~ method - 1,
#         slab = as.character(citation), data = data(), method = "REML")      
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
    
    ggplot(data(), aes(x = d, y = d_var,
                       colour = procedure, fill = procedure)) +
      geom_point() +
      geom_vline(xintercept = 0, linetype = "dotted", color = "grey") +
#      geom_vline(aes(xintercept = b), data = d) + 
#      geom_polygon(aes(x = x, y = y, alpha = 0.005),
#                   data = es_funnel) +
      facet_wrap(~ procedure, ncol = 2) +
      scale_colour_brewer(guide = "none", palette = "Set1") +
      scale_fill_brewer(guide = "none", palette = "Set1") +
      scale_alpha_continuous(guide = "none") +
      xlab("\nEffect Size") +
#      scale_y_continuous(name = "Standard Error\n",
#                         limit = c(0,1)) +
      ylab("Effect Size Variance\n") +
      theme_bw(base_size=14) +
      theme(text = element_text(family = "Open Sans"))
  }, height = 650)
  
})