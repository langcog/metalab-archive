library(shiny)
library(shinythemes)

shinyUI(fluidPage(
  
  theme = shinytheme("spacelab"),
  
  titlePanel("Meta Analysis Visualization"),
  br(),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("dataset", h4("Dataset"),
                  choices = list("Phonetic Discrimination" = "inphondb",
                                 "Word Segmentation" = "inworddb",
                                 "Mutual Exclusivity" = "mutual_exclusivity")),
      selectInput("moderator", h4("Moderator"),
                  choices = list("None" = "none",
                                 "Age" = "mean_age"))
    ), 
    
    mainPanel(
      width = 9,
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"),
      tabsetPanel(
        tabPanel("Scatter Plot", plotOutput("scatter")),
        tabPanel("Violin Plot", plotOutput("violin")),
        tabPanel("Forest Plot", plotOutput("forest", height = "150%")),
        
        tabPanel("Power Analysis",
                 br(),
                 fluidRow(
                   column(3, uiOutput("method")),
                   column(3, numericInput("sig.level", "Significance Level",
                                          value = 0.05, step = 0.01)),
                   column(3, numericInput("power", "Power",
                                          value = 0.8, step = 0.1))
                 ),
                 uiOutput("effect_size"),
                 uiOutput("sample_size")
        )
      )
    )
  )
))