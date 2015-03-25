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

map_fields <- function(dataset) {  
  df <- read.csv(paste0('data/', dataset, '.csv')) 
  
  if (dataset == "inphondb") {
    df <- df %>% rename(method = method,
                  effect_size = EffectSize,
                  ns = nb.included,
                  paper = bibliographical.reference,
                  mean_age = mean.age.days)
  } else if (dataset == "inworddb") {
    df <- df %>% 
      mutate(paper = str_c(Authors,as.character(JnlYear))) %>%
      rename(method = Method,
             effect_size = ES,
             ns = Included,
             paper = paper, 
             mean_age = meanAge)
  } else if (dataset == "mutual_exclusivity") {
    df <- df %>% rename(method = DV.type,
                  effect_size = d,
                  ns = N,
                  paper = paper_key,
                  mean_age = age_mean..months.) %>%
      mutate(mean_age = mean_age * 30)
  } 
      
  df <- df %>% 
    select(method, effect_size, ns, mean_age, paper) %>%
    mutate(id = 1:length(method)) %>%
    filter(!is.na(effect_size))

  return(df)
}

shinyServer(function(input, output) {
  
  ################# REACTIVES FOR SCATTER PLOT #################
  data <- reactive({    
    map_fields(input$dataset) 
  })
  
  reactive({
    print(data())
    
    all_values <- function(x) {
      if(is.null(x)) return(NULL)
      row <- data()[data()$id == x$id, ]
      paste0(names(row), ": ", format(row), collapse = "<br />")
    }
    
    data() %>% 
      ggvis(x = ~mean_age, y = ~effect_size, key := ~id,
            stroke = ~method, fill= ~method) %>%
      layer_points(size = ~ns) %>%
      add_relative_scales() %>%
      add_legend(c("stroke",  "fill")) %>%
      hide_legend("size") %>% # can't figure out add legend for a different position
      add_tooltip(all_values, on=c("hover","click")) 
#     %>%
# #       group_by(method) %>%
#       layer_model_predictions(model = "lm", 
#                               formula = effect_size ~ log(mean_age), 
#                               se = TRUE)
  }) %>% bind_shiny("scatter")
  
  ################# REACTIVES FOR VIOLIN PLOT #################
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
  
  ################# REACTIVES FOR POWER ANALYSIS #################
  method <- reactive({
    ifelse(is.na(input$method), "All", input$method)
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
})