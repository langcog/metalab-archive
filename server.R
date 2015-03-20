library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)
library(pwr)

input <- list(dataset = "inworddb", method = "HPP", sig.level = 0.05, power = 0.8)

map_fields <- function(dataset, df) {
  if (dataset == "inphondb") {
    df %>% rename(method = method,
                  effect_size = EffectSize,
                  n = nb.included,
                  mean_age = mean.age.days)
  } else if (dataset == "inworddb") {
    df %>% rename(method = Method,
                  effect_size = ES,
                  n = Included,
                  mean_age = meanAge)
  }
}

shinyServer(function(input, output) {

  data <- reactive({
    read.csv(paste0('data/', input$dataset, '.csv')) %>%
      map_fields(input$dataset, .) %>%
      filter(!is.na(effect_size)) %>%
      select(method, effect_size, n, mean_age)
  })
  
  output$method <- renderUI({
    selectizeInput("method", "Method", choices = levels(unique(data()$method)))
  })
  
  effect_size <- reactive({
    summary <- data() %>%
      filter(method == input$method) %>%
      summarise(mean.effect = mean(effect_size))
    summary[[1]]
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
  
})