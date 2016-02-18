library(shinydashboard)

header <- dashboardHeader(title = "MetaLab")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Overview", tabName = "overview",
             icon = icon("home", lib = "glyphicon")),
    menuItem("Documentation", tabName = "documentation",
             icon = icon("file", lib = "glyphicon")),
    menuItem("Visualizations", tabName = "visualizations",
             icon = icon("signal", lib = "glyphicon")),
    menuItem("Reports", tabName = "reports",
             icon = icon("folder-open", lib = "glyphicon")),
    menuItem("Power Analysis", tabName = "power_analysis",
             icon = icon("flash", lib = "glyphicon"))
  )
)

body <- dashboardBody(
  # replace with includeCSS
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  tabItems(
    tabItem(tabName = "overview",
            wellPanel(
              div(class = "text-center",
                  fluidRow(column(width = 12, h1("MetaLab"),
                                  p(class = "lead", "A tool for power analysis and experimental",
                                    br(), "planning in language acquisition research")
                  )
                  )
              )
            ),
            br(),
            h3("Meta-analyses currently in MetaLab:"),
            fluidRow(
#              column(width = 12,
                     box(width = 6,
                       fluidRow(
                         column(width = 3,
                                img(src = "images/dinosaur.png", width = 100)),
                         column(width = 9,
                                h3("Phonemic discrimination"))
                       )
                     ),
                     box(width = 6,
                         fluidRow(
                           column(width = 3,
                                  img(src = "images/dinosaur.png", width = 100)),
                           column(width = 9,
                                  h3("Word segmentation"))
                         )
                     ),
                     box(width = 6,
                         fluidRow(
                           column(width = 3,
                                  img(src = "images/dinosaur.png", width = 100)),
                           column(width = 9,
                                  h3("Infant directed speech preference"))
                         )
                     ),
                     box(width = 6,
                         fluidRow(
                           column(width = 3,
                                  img(src = "images/dinosaur.png", width = 100)),
                           column(width = 9,
                                  h3("Pointing"))
                         )
                     ),
                     box(width = 6,
                         fluidRow(
                           column(width = 3,
                                  img(src = "images/zot.png", width = 100)),
                           column(width = 9,
                                  h3("Mutual exclusivity"))
                         )
                     ),
                     box(width = 6,
                       fluidRow(
                         column(width = 3,
                                img(src = "images/dinosaur.png", width = 100)),
                         column(width = 9,
                                h3("Label advantage in concept learning"))
                       )
                     )
#              )
#               column(width = 12,
#                      box(
#                        fluidRow(
#                          column(width = 3,
#                                 img(src = "images/dinosaur.png", width = 70, height = 100)),
#                          column(width = 9,
#                                 h2("Mutual exclusivity"))
#                        )
#                      )
#               )
            )
    #includeRmd("../overview.Rmd", list('datasets' = datasets))
  ),
  tabItem(tabName = "documentation"),
  tabItem(tabName = "visualizations",
          fluidRow(
            column(width = 6,
                   box(width = NULL, status = "danger",
                       selectInput("dataset_name", label = "Dataset", choices = datasets$name),
                       uiOutput("moderator_input")
                       # checkboxGroupInput("moderators", label = "Moderators",
                       #                    choices = list("Age" = "mean_age",
                       #                                   "Exposure phase" = "exposure_phase",
                       #                                   "Response mode" = "response_mode"),
                       #                    inline = TRUE)
                   ),
                   box(width = NULL, status = "danger", plotOutput("scatter")),
                   box(width = NULL, status = "danger", plotOutput("violin")),
                   box(width = NULL, status = "danger", plotOutput("funnel"))
            ),
            column(width = 6,
                   fluidRow(valueBoxOutput("studies_box"),
                            valueBoxOutput("effect_size_box"),
                            valueBoxOutput("effect_size_var_box")),
                   fluidRow(box(width = NULL, status = "danger", plotOutput("forest")))
            )
          )
  ),
  tabItem(tabName = "reports"),
  tabItem(tabName = "power_analysis")
)
)

dashboardPage(skin = "red", header, sidebar, body)
