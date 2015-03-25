library(shiny)
library(shinyapps)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggvis)
library(pwr)
library(magrittr)
library(stringr)


#input <- list(dataset = "inphondb", method = "AEM", sig.level = 0.05, power = 0.8)

map_fields <- function(dataset, df) {
  if (dataset == "inphondb") {
    df %>% rename(method = method,
                  effect_size = EffectSize,
                  ns = nb.included,
                  paper = bibliographical.reference,
                  mean_age = mean.age.days)
  } else if (dataset == "inworddb") {
    df %>% 
      mutate(paper = str_c(Authors,as.character(JnlYear))) %>%
      rename(method = Method,
             effect_size = ES,
             ns = Included,
             paper = paper, 
             mean_age = meanAge)
  } else if (dataset == "mutual_exclusivity") {
    df %>% rename(method = DV.type,
                  effect_size = d,
                  ns = N,
                  paper = paper_key,
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
      select(method, effect_size, paper, ns, mean_age)
  })
  
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
               power = input$power, type = c("two.sample", "one.sample", "paired"))$ns
  })
  
  output$sample_size <- renderText({
    sprintf("Estimated sample size is %s subjects in each group.", ceiling(sample_size()))
  })
  
  
  
  data %>% 
    mutate(numsub = ns) %>%
    ggvis(x = ~mean_age, y = ~effect_size, 
          stroke = ~method, fill= ~method) %>%
    layer_points(size = ~numsub) %>%
    add_relative_scales() %>%
    add_legend(c("stroke",  "fill")) %>%
    add_legend("size",
               properties = legend_props(
                 legend = list(
                   x = scaled_value("x_rel", .9),
                   y = scaled_value("y_rel", .6)))) %>%
#     add_tooltip(function(df) {df$paper}) %>%
    group_by(method) %>%
    layer_model_predictions(model = "lm", 
                            formula = effect_size ~ log(mean_age), 
                            se = TRUE) %>%
    bind_shiny("scatter")

  #   reactive({
#     data %>% 
#       ggvis(x = ~mean_age, y = ~effect_size, 
#             fill = ~method, size = ~n) %>% 
#       layer_points() %>%
# #   layer_smooths() %>%
# #       add_tooltip(function(df) {df$paper}) %>%
#       bind_shiny("scatter")
#   })
  
#     ggplot(data(), aes(x = mean_age, y = effect_size, colour = method)) +
#       geom_point(aes(size = n)) +
#       scale_colour_brewer(name = "Method", palette = "Set1") +
#       scale_size_continuous(name = "n") +
#       xlab("\nMean Subject Age (Days)") +
#       ylab("Effect Size\n") +
#       theme_bw(base_size=14) +
#       theme(text = element_text(family = "Open Sans"))
    

  
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