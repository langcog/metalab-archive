library(shiny)
library(shinythemes)

shinyUI(navbarPage("MetaBaby",
  theme = shinytheme("united"),
  
  tabPanel("Overview",
           column(width=10, offset=1,
                  tags$style(type="text/css", "h1 { font-size: 400%; }"),
#              tags$div(class="jumbotron",
              wellPanel(
              h1("MetaBaby"),
              fluidRow(
                column(width=1,
                       tags$span(class="fa-stack fa-2x",
                                 icon("folder-o", "fa-stack-2x"),
                                 icon("child", "fa-stack-1x"))
                ),
                column(width=11, h4("A tool for power analysis and experimental", br(), "planning in language acquisition research")))
              ),
#               ),
             includeRmd("overview.Rmd", datasets)
           )
  ),
  
  tabPanel("Individual Meta-Analyses", 
             
  sidebarLayout(
    sidebarPanel(
      width = 4,
      inputPanel(
        uiOutput("datasets")
      ),
      inputPanel(
        h4("Moderators"),
        checkboxInput("mod_method", label = "Method"),
        checkboxInput("mod_procedure", label = "Procedure"),
        checkboxInput("mod_mean_age", label = "Mean Age")
      )
    ), 
    
    mainPanel(
      width = 8,
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
                   column(4, sliderInput("N", label = h5("Sample size"),
                                         min = 5, max = 120, value = 20)),
                   column(4,
                          conditionalPanel(
                            condition = "input.mod_mean_age",
                            uiOutput("mean_age")))
                 ),                 fluidRow(
                   column(3,
                          conditionalPanel(
                            condition = "input.mod_method",
                            uiOutput("method"))),
                   column(4,
                          conditionalPanel(
                            condition = "input.mod_procedure",
                            uiOutput("procedure")))
                 ),
                 uiOutput("effect_size"),
                 plotOutput("power")
        )
      )
    )
  )
),

tabPanel("Meta-Meta-Analysis",
         sidebarLayout(
           sidebarPanel(
             width = 4,
             uiOutput("meta_datasets")
             ),
           mainPanel(
             width = 8,
             plotOutput("metameta", width = "100%")
           )
         )
      )
  )
)