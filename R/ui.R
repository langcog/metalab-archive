library(shiny)
library(shinythemes)

shinyUI(fluidPage(
  
  theme = shinytheme("spacelab"),
  
  titlePanel("Meta Analysis Visualization"),
  br(),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      uiOutput("datasets"),
      br(),
      h4("Moderators"),
      checkboxInput("mod_method", label = "Method"),
      checkboxInput("mod_procedure", label = "Procedure"),
      checkboxInput("mod_mean_age", label = "Mean Age")
#      uiOutput("moderators")
    ), 
    
    mainPanel(
      width = 9,
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"),
      tabsetPanel(
        tabPanel("Scatter Plot", plotOutput("scatter")),
        #        tabPanel("Violin Plot", plotOutput("violin")),
        tabPanel("Forest Plot", plotOutput("forest", width = "100%", height = "auto")),
        tabPanel("Funnel Plot", plotOutput("funnel")),        
        tabPanel("Power Analysis",
                 br(),
                 fluidRow(
                   column(4,
                          conditionalPanel(
                            condition = "input.mod_method",
                            uiOutput("method"))),
                   column(4,
                          conditionalPanel(
                            condition = "input.mod_procedure",
                            uiOutput("procedure"))),
                   column(4,
                          conditionalPanel(
                            condition = "input.mod_mean_age",
                            uiOutput("mean_age")))
                 ),
                 uiOutput("effect_size"),
                 plotOutput("power_plot")
                 #                 uiOutput("sample_size")
        )
      )
    )
  )
)
)