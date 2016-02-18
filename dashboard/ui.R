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
    menuItem("Power Analysis", tabName = "power",
             icon = icon("flash", lib = "glyphicon"))
  )
)

tab_overview <- tabItem(
  tabName = "overview",
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
    map(1:nrow(datasets), function(i) {
      dataset <- datasets[i,]
      box(width = 6,
          fluidRow(
            column(width = 3,
                   img(src = dataset$src, width = 100)),
            column(width = 9,
                   h3(dataset$name))
          )
      )
    })
  )
)

tab_documentation <- tabItem(
  tabName = "documentation",
  includeRmd("../overview.Rmd", list("datasets" = datasets))
)

tab_visualizations <- tabItem(
  tabName = "visualizations",
  fluidRow(
    column(width = 6,
           box(width = NULL, status = "danger",
               selectInput("dataset_name", label = "Dataset", choices = datasets$name),
               uiOutput("moderator_input")
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
)

tab_reports <- tabItem(
  tabName = "reports"
)

tab_power <- tabItem(
  tabName = "power"
)

body <- dashboardBody(
  includeCSS("www/custom.css"),
  tabItems(tab_overview, tab_documentation, tab_visualizations,
           tab_reports, tab_power)
)

dashboardPage(skin = "red", header, sidebar, body)
