library(shiny)
library(shinyapps)
library(tidyr)
library(dplyr)
library(ggplot2)
library(pwr)
library(magrittr)
library(metafor)
source('meta_helper.R')

#input <- list(dataset = "inphondb", method = "AEM", sig.level = 0.05, power = 0.8)

map_fields <- function(dataset, df) {
  if (dataset == "inphondb") {
    df %>% rename(method = method,
                  effect_size = EffectSize,
                  n = nb.included,
                  mean_age = mean.age.days) %>%
      mutate(d_var = NA)
  } else if (dataset == "inworddb") {
    df %>% rename(method = Method,
                  effect_size = ES,
                  n = Included,
                  mean_age = meanAge) %>%
        mutate(d_var = NA)
  } else if (dataset == "mutual_exclusivity") {
    df %>% rename(method = DV.type,
                  effect_size = d_calculate,
                  n = N,
                  mean_age = age_mean..months.) %>%
      mutate(mean_age = mean_age * 30)
  }
}


shinyServer(function(input, output) {
  
  method <- reactive({
    ifelse(is.na(input$method), "All", input$method)
  })
  
  data <- reactive({
    read.csv(paste0('data/', input$dataset, '.csv')) %>%
      map_fields(input$dataset, .) %>%
      filter(!is.na(effect_size)) %>%
      select(method, effect_size, n, mean_age, d_var)
  })
  
  moderator <- reactive({input$moderator})

  output$method <- renderUI({
    selectizeInput("method", "Method", choices = c("All", levels(unique(data()$method))),
                   select = "All")
  })
  
  effect_size <- reactive({
    filtered <- data()
    if (method() != "All") {
      filtered %<>% filter(method == input$method)
    }
    summarise(filtered, mean.effect = mean(effect_size))[[1]]
  })
  
  output$effect_size <- renderText({
    sprintf("Estimated effect size is %s.", round(effect_size(), 2))
  })
  
  sample_size <- reactive({
    pwr.t.test(d = effect_size(), sig.level = input$sig.level,
               power = input$power, type = c("two.sample", "one.sample", "paired"))$n
  })
  
  output$sample_size <- renderText({
    sprintf("Estimated sample size is %s subjects in each group.", ceiling(sample_size()))
  })
  
  output$scatter <- renderPlot({
    ggplot(data(), aes(x = mean_age, y = effect_size, colour = method)) +
      geom_point(aes(size = n)) +
      scale_colour_brewer(name = "Method", palette = "Set1") +
      scale_size_continuous(name = "n") +
      xlab("\nMean Subject Age (Days)") +
      ylab("Effect Size\n") +
      theme_bw(base_size=14) +
      theme(text = element_text(family = "Open Sans"))
  })
  
  output$violin <- renderPlot({
    ggplot(data(), aes(x = factor(method), y = effect_size, colour = method)) +
      geom_jitter(height = 0) +
      geom_violin() +
      scale_colour_brewer(name = "", palette = "Set1") +
      #      scale_size_continuous(name = "n") +
      xlab("\nMethod") +
      ylab("Effect Size\n") +
      theme_bw(base_size=14) +
      theme(text = element_text(family = "Open Sans"))
  })
  
  output$forest <- renderPlot({
    # get model
    if (moderator() == "none") {
      model = rma(effect_size, vi = d_var, data = data(), method = "REML")      
    } else {
      model = rma(effect_size ~ eval(parse(text=moderator())), vi = d_var, data = data(), method = "REML")     
    }
    
    # plot
    forest(model,
           mlab = "Grand effect size",
           xlab ="Effect size estimate",
           annotate = F)
  }, height = 800, width = 600)

})