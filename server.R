library(shiny)
library(shinyapps)
library(tidyr)
library(dplyr)
library(ggplot2)
library(pwr)
library(magrittr)
library(metafor)

#input <- list(dataset = "inphondb", method = "All", sig.level = 0.05, power = 0.8,
#              moderator = "none")

map_fields <- function(dataset, df) {
  if (dataset == "inphondb") {
    df %>% rename(method = method,
                  effect_size = EffectSize,
                  effect_size_se = ES.SE,
                  effect_size_weight = ES.w,
                  n = nb.included,
                  mean_age = mean.age.days) %>%
      mutate(effect_size_var = effect_size_se ^ 2) %>%
      rowwise() %>%
      mutate(method_name = switch(method,
                                  "AEM" = "Anticipatory Eye Movement",
                                  "CF-HAB" = "Central Fixation & Habituation",
                                  "CF-SA" = "Central Fixation & Stimulus Alternation",                                  
                                  "CHT" = "Conditioned Head-Turn",
                                  "HAS" = "High Amplitude Sucking",
                                  "HPP-FAM" = "Head-Turn Preference & Familiarization"))
  } else if (dataset == "inworddb") {
    df %>% rename(method = Method,
                  effect_size = ES,
                  effect_size_se = ES.SE,
                  effect_size_weight = ES.W,
                  n = Included,
                  mean_age = meanAge) %>%
      mutate(effect_size_var = effect_size_se ^2) %>%
      rowwise() %>%
      mutate(method_name = switch(method,
                                  "HPP-FAM" = "Head-Turn Preference",
                                  "Other" = "Other"))
  } else if (dataset == "mutual_exclusivity") {
    df %>% rename(method = DV.type,
                  effect_size = d_calculate,
                  effect_size_var = d_var,
                  n = N,
                  mean_age = age_mean..months.) %>%
      mutate(mean_age = mean_age * 30,
             effect_size_se = sqrt(effect_size_var),
             effect_size_weight = 1.0/effect_size_var,
             method_name = method)
  }
}


shinyServer(function(input, output) {
  
  method <- reactive({
    ifelse(is.null(input$method), "All", input$method)
  })
  
  data <- reactive({
    read.csv(paste0('data/', input$dataset, '.csv')) %>%
      map_fields(input$dataset, .) %>%
      filter(!is.na(effect_size)) %>%
      select(method, method_name, effect_size, effect_size_weight, effect_size_var, n, mean_age)
  })
  
  moderator <- reactive({input$moderator})

  output$method <- renderUI({
    selectizeInput("method", "Method", choices = c("All", unique(data()$method_name)),
                   select = "All")
  })
  
  effect_size <- reactive({
    filtered <- data() %>%
      filter(!is.na(effect_size_weight))
    if (method() != "All") {
      filtered %<>% filter(method_name == input$method)
    }
    summarise(filtered, mean.effect = weighted.mean(effect_size, effect_size_weight))[[1]]
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
      model = rma(effect_size, vi = effect_size_var, data = data(), method = "REML")      
    } else {
      model = rma(effect_size ~ eval(parse(text=moderator())), vi = effect_size_var, data = data(), method = "REML")     
    }
    
    # plot
    forest(model,
           mlab = "Grand effect size",
           xlab ="Effect size estimate",
           annotate = F)
  }, height = 800, width = 600)

  output$funnel <- renderPlot({
    ggplot(data(), aes(x = effect_size, y = n, colour = method)) +
      geom_point() +
      facet_wrap(~ method_name, ncol = 2) +
      scale_colour_brewer(name = "Method", palette = "Set1") +
      xlab("\nEffect Size") +
      ylab("Sample Size\n") +
      theme_bw(base_size=14) +
      theme(text = element_text(family = "Open Sans"))
  }, height = 650)
  
})