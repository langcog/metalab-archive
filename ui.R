library(shiny)
library(shinythemes)

shinyUI(fluidPage(
  
  theme = shinytheme("spacelab"),
  
  titlePanel("Meta Analysis Visualization"),
  br(),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      selectInput("dataset", h4("Dataset"),
                  choices = list("Phonetic Discrimination" = "inphondb",
                                 "Word Segmentation" = "inworddb"))
    ),
    
    mainPanel(
      width = 8,
      tabsetPanel(
        tabPanel("Scatter Plot", plotOutput("scatter")),
        tabPanel("Violin Plot", plotOutput("violin"))
#         tabPanel("Power Analysis",
#                  br(),
#                  fluidRow(
#                    column(3, renderUI("method"))#,
#                    #column(3, )
#                  )
#         )
      )
    )
  )
))