library(shinydashboard)
options(shiny.trace=TRUE)

header <- dashboardHeader(title = "MetaLab")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Overview", tabName = "overview",
             icon = icon("home", lib = "glyphicon")),
    menuItem("Documentation", tabName = "documentation",
             icon = icon("file", lib = "glyphicon")),
    menuItem("Visualizations", tabName = "visualizations",
             icon = icon("signal", lib = "glyphicon")),
    menuItem("Power Analysis", tabName = "power",
             icon = icon("flash", lib = "glyphicon")),
    menuItem("Reports", tabName = "reports",
             icon = icon("folder-open", lib = "glyphicon"))
  )
)

#############################################################################
# OVERVIEW

tab_overview <- tabItem(
  tabName = "overview",
  wellPanel(
    div(class = "text-center",
        fluidRow(column(
          width = 12,
          h1("MetaLab"),
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
      box(
        width = 6,
        fluidRow(
          column(width = 3, img(src = dataset$src, width = 100)),
          column(width = 9, h3(dataset$name))
        )
      )
    })
  )
)

#############################################################################
# DOCUMENTATION

tab_documentation <- tabItem(
  tabName = "documentation",
  includeRmd("../overview.Rmd", list("datasets" = datasets))
)

#############################################################################
# VISUALIZATIONS

tab_visualizations <- tabItem(
  tabName = "visualizations",
  fluidRow(
    column(
      width = 6,
      box(width = NULL, status = "danger",
          selectInput("dataset_name", label = "Dataset",
                      choices = datasets$name),
          uiOutput("moderator_input")
      ),
      box(width = NULL, status = "danger", plotOutput("scatter")),
      box(width = NULL, status = "danger", plotOutput("violin")),
      box(width = NULL, status = "danger", plotOutput("funnel"))
    ),
    column(
      width = 6,
      fluidRow(valueBoxOutput("studies_box"),
               valueBoxOutput("effect_size_box"),
               valueBoxOutput("effect_size_var_box")),
      fluidRow(box(width = NULL, status = "danger", plotOutput("forest")))
    )
  )
)

#############################################################################
# POWER ANALYSIS

tab_power <- tabItem(
  tabName = "power",
  fluidRow(
    column(
      width = 6,
      box(width = NULL, status = "danger",
          sliderInput("d", p("Effect size (Cohen's d)"),
                      min = 0, max = 2.5, value = .5, step = .1),
          sliderInput("N", p("Number of infants per group (N)"),
                      min = 4, max = 120, value = 16, step = 2))),
    column(
      width = 6,
      box(width = NULL, status = "danger",
          radioButtons("control", p("Conditions"),
                       choices = list(
                         "Experimental only" = FALSE,
                         "Experimental + negative control" = TRUE)),
          radioButtons("interval", p("Type of error bars"),
                       choices = list("Standard error of the mean" = "sem",
                                      "95% confidence interval" = "ci"),
                       selected = "ci"),
          actionButton("go", label = "Sample Again")))),
  fluidRow(
    column(
      width = 6,
      box(width = NULL, status = "danger", plotOutput("bar")),
      box(width = NULL, status = "danger", textOutput("stat"))),
    column(
      width = 6,
      box(width = NULL, status = "danger",
          plotOutput("power"), br(),
          p("Plot shows statistical power to detect a difference between
            conditions at p < .05 with the selected effect size, plotted by
            conditions. Red dot shows power with currently selected N. Dashed
            lines show 80% power and necessary sample size.")
      )
    )
  )
)

#############################################################################
# REPORTS

tab_reports <- tabItem(
  tabName = "reports"
)

#############################################################################
# OTHER FORMATTING

body <- dashboardBody(
  includeCSS("www/custom.css"),
  tabItems(tab_overview, tab_documentation, tab_visualizations,
           tab_power, tab_reports)
)

dashboardPage(skin = "red", header, sidebar, body)
