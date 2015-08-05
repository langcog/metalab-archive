library(shiny)
library(shinythemes)

shinyUI(navbarPage(name,
  theme = shinytheme("united"),
  
  tabPanel("Overview",
           column(width=10, offset=1,
                  tags$style(type="text/css", "h1 { font-size: 400%; }"),
              wellPanel(
                div(class="text-center",
              
              fluidRow(
                column(width=12, h1(name), p(class="lead", "A tool for power analysis and experimental", br(), "planning in language acquisition research"))
              )
              )),
             includeRmd("overview.Rmd", list('datasets' = datasets,
                                             'name' = name))
           )
  ),
  
  tabPanel("Individual Meta-Analyses", 
             
  sidebarLayout(
    sidebarPanel(
      width = 3,
      inputPanel(
        #uiOutput("datasets")
        selectInput("dataset_name", label = h4("Dataset"), choices = datasets$name)
      ),
      inputPanel(
        h4("Moderators"),
        checkboxInput("mod_mean_age", label = "Mean Age"),
        conditionalPanel(condition = "output.include_procedure",
                         checkboxInput("mod_procedure", label = "Procedure")),
        conditionalPanel(condition = "output.include_response_mode",
                         checkboxInput("mod_response_mode", label = "Response Mode"))
      )
    ), 
    
    mainPanel(
      width = 9,
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"),
      tabsetPanel(
        tabPanel("Forest Plot", plotOutput("forest", width = "100%", height = "auto")),
        
        tabPanel("Scatter Plot", plotOutput("scatter")),
        tabPanel("Violin Plot", plotOutput("violin")),
        tabPanel("Funnel Plot", plotOutput("funnel")),        
        tabPanel("Power Analysis",
                 br(),
                 inputPanel(
                    sliderInput("N", label = h5("Sample size"),
                                min = 5, max = 120, value = 20),
                    conditionalPanel(
                      condition = "input.mod_mean_age",
                      uiOutput("mean_age")),
                    conditionalPanel(
                      condition = "input.mod_procedure",
                      uiOutput("procedure")),
                    conditionalPanel(
                      condition = "input.mod_response_mode",
                      uiOutput("response_mode"))
                 ),
                 br(),
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
             width = 3,
             uiOutput("meta_datasets")
             ),
           mainPanel(
             width = 9,
             plotOutput("metameta", width = "100%")
           )
         )
      )
  )
)