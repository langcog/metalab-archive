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
    menuItem("Contribute", tabName = "contribute",
             icon = icon("upload")),
    menuItem("Team", tabName = "team",
             icon = icon("users")),
    menuItem("Source code", icon = icon("file-code-o"),
             href = "https://github.com/langcog/metalab/")
  ),
  tags$footer(
    class = "footer",
    p("Questions or comments?", br(),
      a("metalab-project@googlegroups.com",
        href = "mailto:metalab-project@googlegroups.com"),
      class = "small", align = "center")
  )
)

#############################################################################
# HOME

dataset_box <- function(i) {
  dataset <- datasets[i,]
  box(
    width = 12, status = "danger", solidHeader = TRUE,
    fluidRow(
      column(width = 3,
             img(src = dataset$src, width = "100%", class = "dataset-img")),
      column(width = 9, class = "dataset-txt",
             h4(strong(dataset$name)), p(dataset$short_desc)))
  )
}

tab_home <- tabItem(
  tabName = "home",
  div(class = "text-center",
      fluidRow(
        column(
          width = 12,
          h1("MetaLab", class = "jumbo"),
          p(class = "lead",
            "Interactive tools for community-augmented meta-analysis,", br(),
            "power analysis, and experimental planning in language acquisition
            research")
        )
      )
  ),
  fluidRow(
    column(width = 2),
    valueBox(nrow(datasets),
             "Meta-analyses", color = "red", width = 2, icon = icon("cubes")),
    valueBox(datasets$num_papers %>% sum(na.rm = TRUE) %>% as.integer(),
             "Papers", color = "red", width = 2, icon = icon("file-text-o")),
    valueBox(datasets$num_experiments %>% sum(na.rm = TRUE) %>% as.integer(),
             "Effect sizes", color = "red", width = 2, icon = icon("list")),
    valueBox(datasets$num_subjects %>% sum(na.rm = TRUE) %>% as.integer() %>%
               format(big.mark = ","),
             "Participants", color = "red", width = 2, icon = icon("child")),
    column(width = 2)
  ),
  h3("Meta-analyses currently in MetaLab:"),
  fluidRow(
    column(width = 6,
           map(1:ceiling(nrow(datasets)/2), dataset_box)
    ),
    column(width = 6,
           map(ceiling(nrow(datasets)/2 + 1):nrow(datasets), dataset_box)
    )
  )
)

#############################################################################
# DOCUMENTATION

tab_documentation <- tabItem(
  tabName = "documentation",
  tabBox(width = "100%", status = "danger",
         tabPanel("Overview",
                  includeRmd("rmarkdown/overview.Rmd")),
         tabPanel("Datasets",
                  includeRmd("rmarkdown/datasets.Rmd",
                             list("datasets" = datasets))),
         tabPanel("Field Specification",
                  h3("Required fields"),
                  DT::dataTableOutput("req_table"),
                  h3("Optional fields"),
                  DT::dataTableOutput("opt_table"),
                  h3("Derived fields"),
                  DT::dataTableOutput("drv_table")),
         tabPanel("Phonemic Discrimination",
                  includeRmd("rmarkdown/inphondb.Rmd")),
         tabPanel("Word Segmentation",
                  includeRmd("rmarkdown/inworddb.Rmd"))
  )
)

#############################################################################
# CONTRIBUTE

tab_contribute <- tabItem(
  tabName = "contribute",
  tabBox(width = "100%", status = "danger",
         tabPanel("Motivation and Background",
                  includeRmd("rmarkdown/background.Rmd")),
         tabPanel("Building a MA",
                  includeRmd("rmarkdown/building.Rmd")),
         tabPanel("Contribute to MetaLab",
                  includeRmd("rmarkdown/contribute.Rmd"))
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
                           choices = datasets$name)),
        column(width = 7,
               downloadButton("table_download_data", "Download data",
                              class = "btn-xs pull-right"))
      ),
      DT::dataTableOutput("dataset_table")
  )
)

#############################################################################
# VISUALIZATIONS

ma_choices <- c("Random effects with maximum likelihood (recommended)" = "REML",
                "Fixed effects" = "FE",
                "Empirical Bayes" = "EB")

scatter_choices <- c("Locally-linear regression (loess)" = "loess",
                     "Weighted linear model (lm)" = "lm")

es_choices <- c("Cohen's d" = "d",
                "Hedges' g" = "g",
                "Pearson r" = "r",
                "Log odds ratio" = "log_odds")

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
          selectInput("ma_method", label = "Meta-analytic model",
                      choices = ma_choices, selected = "REML"),
          fluidRow(
            column(
              width = 4,
              selectInput("es_type", label = "Effect size type",
                          choices = es_choices, selected = "d")
            ),
            column(
              width = 8,
              uiOutput("moderator_input")
            )
          )
      ),
      conditionalPanel(
        condition = "output.longitudinal == 'FALSE'",
        box(width = NULL, status = "danger",
            fluidRow(
              column(
                width = 10,
                p(strong("Scatter plot"), "of effect sizes over age")),
              column(
                width = 2,
                downloadButton("download_scatter", "Save",
                               class = "btn-xs pull-right"))),
            column(
              width = 7,
              # uiOutput("select_scatter_curve")),
              selectInput("scatter_curve", label = "Curve type",
                          choices = scatter_choices, selected = "loess")),
            plotOutput("scatter"), height = 530)
      ),
      box(width = NULL, status = "danger",
          fluidRow(
            column(width = 10,
                   p(strong("Funnel plot"), "of bias in effect sizes")),
            column(width = 2,
                   downloadButton("download_funnel", "Save",
                                  class = "btn-xs pull-right"))),
          plotOutput("funnel"),
          div(class = "text-center", textOutput("funnel_test"))),
      box(width = NULL, status = "danger",
          fluidRow(
            column(width = 10,
                   p(strong("Violin plot"), "of effect size density")),
            column(width = 2,
                   downloadButton("download_violin", "Save",
                                  class = "btn-xs pull-right"))),
          plotOutput("violin", height = "auto")
      )),
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
                          choices = c("effect size" = "effects",
                                      "model estimate" = "estimate",
                                      "alphabetical" = "unique_ID",
                                      "chronological" = "year"))),
            plotOutput("forest", height = "auto")),
        box(width = NULL, status = "danger",
            fluidRow(
              column(width = 12,
                     p(strong("Meta-analytic model summary")),
                     tabsetPanel(
                       tabPanel("Plot",
                                plotOutput("forest_summary", height = "auto")),
                       tabPanel("Model",
                                p(verbatimTextOutput("forest_summary_text")))
                     )
              )
            )
        )
      )
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
          p("Select a meta-analysis and a set of moderators to see statistical
             power estimates using the estimated effect size for that
             phenomenon. (Currently supports age only)."),
          selectInput("dataset_name_pwr", "Meta-analysis",
                      choices = datasets$name),
          uiOutput("pwr_moderator_input"),
          uiOutput("pwr_moderator_choices"),
          strong("Power by number of participants"),
          p("Statistical power to detect a difference between
            conditions at p < .05. Dashed line shows 80% power, dotted line
            shows necessary sample size to achieve that level of power."),
          plotOutput("power"))),
    column(
      width = 6,
      box(width = NULL, status = "danger", solidHeader = TRUE,
          title = "Experiment simulation",
          p(
            "Run a simulation of a looking-time experiment, choosing an effect
             size and a number of participants per group. See the results of
             statistical comparisons for within-subjects effects (t-test) and
             for comparison with a negative control group (ANOVA interaction)."
          ),
          sliderInput("N", "Number of infants per group (N)",
                      min = 4, max = 120, value = 16, step = 2),
          sliderInput("d_pwr", "Effect size (Cohen's d)",
                      min = 0, max = 2, step = .1,
                      value = .5),
          fluidRow(
            column(
              width = 6,
              radioButtons("control", "Conditions",
                           choices = list("Experimental only" = FALSE,
                                          "Experimental & control" = TRUE))),
            column(
              width = 6,
              radioButtons("interval", "Type of error bars",
                           choices = list("Standard error of the mean" = "sem",
                                          "95% confidence interval" = "ci"),
                           selected = "ci"))
          ),
          selectInput("pwr_bar", "Plot type",
                      choices = list("Bar graph" = TRUE,
                                     "Scatter plot" = FALSE),
                      selected = TRUE),
          actionButton("go", label = "Sample Again"),
          hr(),
          strong("Simulated data"),
          conditionalPanel(condition = "input.pwr_bar == 'FALSE'",
                           plotOutput("pwr_scatter")),
          conditionalPanel(condition = "input.pwr_bar == 'TRUE'",
                           plotOutput("pwr_bar")),
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
  report_url <- sprintf(
    "https://rawgit.com/langcog/metalab/gh-pages/reports/%s.html", report$file
  )
  tabItem(
    tabName = report$file,
    tags$iframe(src = report_url, width = 1000, height = 1200, frameBorder = 0))
})

#############################################################################
# TEAM

person_content <- function(person) {
  box(width = 3, align = "center", status = "danger", solidHeader = TRUE,
      img(src = person$image, width = 180, height = 180),
      a(h4(strong(person$name)), href = person$website, target = "_blank"),
      person$affiliation, br(),
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

tabs <- c(list(tab_home, tab_visualizations, tab_power, tab_data,
               tab_documentation, tab_contribute, tab_team), report_tabs)

body <- dashboardBody(
  includeCSS("www/custom.css"),
  tags$style(type = "text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),
  do.call(tabItems, tabs)
)

dashboardPage(header, sidebar, body, title = "MetaLab", skin = "red")
