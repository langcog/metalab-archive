library(shiny)
library(shinydashboard)
library(DT)

header <- dashboardHeader(title = "MetaLab")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Home", tabName = "home",
             icon = icon("home")),
    menuItem("Visualizations", tabName = "visualizations",
             icon = icon("signal")),
    menuItem("Power Analysis", tabName = "power",
             icon = icon("flash")),
    menuItem("Reports", tabName = "reports",
             icon = icon("folder-open"),
             map(reports, function(report) {
               menuSubItem(report$title, tabName = report$file)
             })),
    menuItem("Browse Raw Data", tabName = "data",
             icon = icon("list")),
    menuItem("Documentation", tabName = "documentation",
             icon = icon("file")),
    menuItem("Team", tabName = "team",
             icon = icon("users")),
    menuItem("Source code", icon = icon("file-code-o"),
             href = "https://github.com/langcog/metalab/")
  ),
  tags$footer(
    class = "footer",
    p("Questions or comments?", br(), a("metalab-project@googlegroups.com",
                                        href = "mailto:metalab-project@googlegroups.com"),
      class = "small", align = "center")
  )
)

#############################################################################
# HOME

tab_home <- tabItem(
  tabName = "home",
#  wellPanel(
    div(class = "text-center",
        fluidRow(column(
          width = 12,
          h1("MetaLab", class = "jumbo"),
          p(class = "lead", "Interactive tools for community-augmented meta-analysis,",
            br(),
            "power analysis, and experimental planning in language acquisition research")
        )
        )
#    )
  ),
  fluidRow(
    column(width = 2),
    valueBox(nrow(datasets),
             "Meta-analyses", color = "red", width = 2, icon = icon("cubes")),
    valueBox(as.integer(sum(as.numeric(datasets$num_papers), na.rm = TRUE)),
             "Papers", color = "red", width = 2, icon = icon("file-text-o")),
    valueBox(as.integer(sum(as.numeric(datasets$num_experiments), na.rm = TRUE)),
             "Effect sizes", color = "red", width = 2, icon = icon("list")),
    valueBox(format(as.integer(sum(as.numeric(datasets$num_subjects), na.rm = TRUE)),
                    big.mark = ","),
             "Participants", color = "red", width = 2, icon = icon("child")),
    column(width = 2)
  ),
  h3("Meta-analyses currently in MetaLab:"),
  fluidRow(
    map(1:nrow(datasets), function(i) {
      dataset <- datasets[i,]
      box(
        width = 6, status = "danger", solidHeader = TRUE,
        fluidRow(
          column(width = 3, img(src = dataset$src, height = 70, width = 110)),
          column(width = 9, h4(strong(dataset$name)),
                 p(dataset$description)))
      )
    })
  )
)

#############################################################################
# DOCUMENTATION

tab_documentation <- tabItem(
  tabName = "documentation",
  tabBox(width = "100%", status = "danger",
         tabPanel("Overview",
                  includeRmd("rmarkdown/overview.Rmd", list("datasets" = datasets))),
         tabPanel("Motivation and Background",
                  includeRmd("rmarkdown/background.Rmd")),
         tabPanel("Building a MA",
                  includeRmd("rmarkdown/building.Rmd")),
         tabPanel("Contribute to MetaLab",
                  includeRmd("rmarkdown/contribute.Rmd")),
         tabPanel("MetaLab Data Specification",
                  h3("Required fields"),
                  DT::dataTableOutput("req_table"),
                  h3("Optional fields"),
                  DT::dataTableOutput("opt_table"),
                  h3("Derived fields"),
                  DT::dataTableOutput("drv_table"))
  )
)

#############################################################################
# DATA

tab_data <- tabItem(
  tabName = "data",
  box(width = "100%", status = "danger",
      fluidRow(
      column(width = 5,
        selectInput("table_dataset_name", label = "Dataset",
                    choices = datasets$name))),
      DT::dataTableOutput("dataset_table")
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
          p("Select a meta-analysis and a set of moderators to see statistical power estimates 
            using the estimated effect size for that phenomenon."),
          selectInput("dataset_name_pwr", "Meta-analysis",
                      choices = datasets$name),
          #           selectInput("standard_pwr", "Or use a standard effect size",
          #                       selected = NA,
          #                       choices = list("Small (.2)"=.2, "Medium (.5)"=.5, "Large (.8)"=.8)),
          # uiOutput("moderator_input_pwr"),
          # 
          strong("Power by number of participants"),
          p("Statistical power to detect a difference between
            conditions at p < .05. Dashed line shows 80% power, dotted line 
            shows necessary sample size to achieve that level of power."),
          plotOutput("power"))),
    column(
      width = 6,
      box(width = NULL, status = "danger", solidHeader = TRUE,
          title = "Experiment simulation",
          sliderInput("N", "Number of infants per group (N)",
                      min = 4, max = 120, value = 16, step = 2),
          sliderInput("d_pwr", "Effect size (Cohen's d)",
                      min = 0, max = 2, step = .1,
                      value = .5),
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

report_tabs <- map(reports, function(report) {
  report_url <- sprintf("https://rawgit.com/langcog/metalab/gh-pages/reports/%s.html",
                        report$file)
  tabItem(
    tabName = report$file,
    tags$iframe(src = report_url, width = 1000, height = 1200, frameBorder = 0))
})

#############################################################################
# TEAM

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

tab_team <- tabItem(
  tabName = "team",
  box(status = "danger", width = "100%",
    h3("Meet the MetaLab team"), br(),
    map(split(people, ceiling(seq_along(people) / 4)),
        function(people_row) {
          fluidRow(
            map(people_row[1:length(people_row)], person_content)
          )
        })
  )
)


#############################################################################
# DASHBOARD STRUCTURE

tabs <- c(list(tab_home, tab_data, tab_visualizations,
               tab_power, tab_documentation, tab_team), report_tabs)

body <- dashboardBody(
  includeCSS("www/custom.css"),
  do.call(tabItems, tabs)
)

dashboardPage(header, sidebar, body, title = "MetaLab", skin = "red")
