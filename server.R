library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)

input <- list(dataset = "inworddb")

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
  
#   mean.effect <- reactive({
#     data %>%
#       select(method, EffectSize) %>%
#       filter(method == input$method, !is.na(EffectSize)) %>%
#       summarise(mean.effect = mean(EffectSize))
#   })
  
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