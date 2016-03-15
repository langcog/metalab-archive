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
          h1("MetaLab", class = "jumbo"),
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

person_content <- function(person) {
  box(width = 3, align = "center", status = "danger", solidHeader = TRUE,
      img(src = person$image, width = 180, height = 180),
      h4(strong(person$name)), person$affiliation, br(),
      a(person$email, href = sprintf("mailto:%s", person$email)), br(),
      tags$small(
      map(unlist(strsplit(person$tags, ", ")),
          ~list(code(.x), br())) %>%
        flatten()
      )
  )
}

tab_documentation <- tabItem(
  tabName = "documentation",
  tabsetPanel(
    tabPanel("Overview",
             includeRmd("rmarkdown/overview.Rmd", list("datasets" = datasets)),
             class = "tab-pane-spaced"),
    tabPanel("Specification", #includeRmd("spec.Rmd"),
             h3("Required fields"),
             DT::dataTableOutput("req_table"),
             h3("Optional fields"),
             DT::dataTableOutput("opt_table"),
             h3("Derived fields"),
             DT::dataTableOutput("drv_table"),
             class = "tab-pane-spaced"),
    tabPanel("People",
             h3("Meet the MetaLab team:"), br(),
             map(split(people, ceiling(seq_along(people)/4)),
                 function(people_row) {
                   fluidRow(
                     map(people_row[1:length(people_row)], person_content)
                   )
                 }),
             class = "tab-pane-spaced"
    )
  )
)

#############################################################################
# VISUALIZATIONS

tab_visualizations <- tabItem(
  tabName = "visualizations",
  fluidRow(
    column(
      width = 6,
      box(width = NULL, status = "danger",
          downloadButton("download_data", "Download data",
                         class = "btn-xs pull-right"),
          br(),
          selectInput("dataset_name", label = "Dataset",
                      choices = datasets$name),
          uiOutput("moderator_input")
      ),
      box(width = NULL, status = "danger",
          fluidRow(
            column(width = 10,
                   p(strong("Scatter plot"), "of effect sizes over age")),
            column(width = 2,
                   downloadButton("download_scatter", "Save",
                                  class = "btn-xs pull-right"))),
          plotOutput("scatter")),
      box(width = NULL, status = "danger",
          fluidRow(
            column(width = 10,
                   p(strong("Violin plot"), "of effect size density")),
            column(width = 2,
                   downloadButton("download_violin", "Save",
                                  class = "btn-xs pull-right"))),
          plotOutput("violin")),
      box(width = NULL, status = "danger",
          fluidRow(
            column(width = 10,
                   p(strong("Funnel plot"), "of bias in effect sizes")),
            column(width = 2,
                   downloadButton("download_funnel", "Save",
                                  class = "btn-xs pull-right"))),
          plotOutput("funnel"),
          div(class = "text-center", textOutput("funnel_test")))
    ),
    column(
      width = 6,
      fluidRow(
        valueBoxOutput("studies_box"),
        valueBoxOutput("effect_size_box"),
        valueBoxOutput("effect_size_var_box")),
      fluidRow(
        box(width = NULL, status = "danger",
            fluidRow(
              column(
                width = 10,
                p(strong("Forest plot"),
                  "of effect sizes and meta-analysis model estimates")),
              column(
                width = 2,
                downloadButton("download_forest", "Save",
                               class = "btn-xs pull-right"))),
            column(
              width = 4,
              selectInput("forest_sort", label = "Sort order",
                          choices = c("model fit" = "effects",
                                      "model estimate" = "estimate",
                                      "alphabetical" = "unique_ID",
                                      "chronological" = "year"))),
            plotOutput("forest", height = "auto")))
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
      box(width = NULL, status = "danger", solidHeader = TRUE,
          title = "Experiment planning",
          selectInput("dataset_name_pwr", "Get effect size from meta-analysis",
                      choices = datasets$name),
          #           selectInput("standard_pwr", "Or use a standard effect size",
          #                       selected = NA,
          #                       choices = list("Small (.2)"=.2, "Medium (.5)"=.5, "Large (.8)"=.8)),
          # uiOutput("moderator_input_pwr"),
          uiOutput("es_slider"),
          strong("Power by number of participants"),
          plotOutput("power"),
          br(),
          p("Statistical power to detect a difference between
            conditions at p < .05 with the selected effect size, plotted by
            conditions. Red dot shows power with currently selected N from simulation. Dashed
            lines show 80% power and necessary sample size."))),
    column(
      width = 6,
      box(width = NULL, status = "danger", solidHeader = TRUE,
          title = "Experiment simulation",
          sliderInput("N", "Number of infants per group (N)",
                      min = 4, max = 120, value = 16, step = 2),
          fluidRow(
            column(width = 6,
                   radioButtons("control", "Conditions",
                                choices = list("Experimental only" = FALSE,
                                               "Experimental & control" = TRUE))),
            column(width = 6,
                   radioButtons("interval", "Type of error bars",
                                choices = list("Standard error of the mean" = "sem",
                                               "95% confidence interval" = "ci"),
                                selected = "ci"))
          ),
          actionButton("go", label = "Sample Again"),
          hr(),
          strong("Simulated data"),
          plotOutput("bar"),
          br(),
          strong("Simulated statistical tests"),
          textOutput("stat")
      )
    )
  )
)

#############################################################################
# REPORTS

tabs <- map(1:nrow(reports),
            ~tabPanel(reports[.x,]$title, includeRmd(reports[.x,]$src),
                      class = "tab-pane-spaced"))

tab_reports <- tabItem(
  tabName = "reports",
  do.call(tabsetPanel, tabs)
)

#############################################################################
# OTHER FORMATTING

body <- dashboardBody(
  includeCSS("www/custom.css"),
  tabItems(tab_overview, tab_documentation, tab_visualizations,
           tab_power, tab_reports)
)

dashboardPage(header, sidebar, body, title = "MetaLab", skin = "red")
