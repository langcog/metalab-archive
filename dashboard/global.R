library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(langcog)

#font <- "Ubuntu"
#theme_set(theme_mikabr(base_family = font) +
#            theme(legend.position = "top",
#                  legend.key = element_blank(),
#                  legend.background = element_rect(fill = "transparent")))

fields <- yaml::yaml.load_file("../metadata/spec.yaml")
fields_derived <- yaml::yaml.load_file("../metadata/spec_derived.yaml") %>%
  transpose() %>%
  simplify_all() %>%
  as_data_frame()

reports <- yaml::yaml.load_file("../metadata/reports.yaml")
people <- yaml::yaml.load_file("../metadata/people.yaml")

includeRmd <- function(path, shiny_data = NULL) {
  shiny:::dependsOnFile(path)
  rmarkdown::render(path, quiet = TRUE)
  includeHTML(gsub(".Rmd", ".html", path))
}

cached_data <- list.files("../data/")

datasets <- jsonlite::fromJSON("../metadata/datasets.json") %>%
  filter(filename %in% cached_data)

load_dataset <- function(filename) {
  feather::read_feather(file.path("..", "data", filename)) %>%
    mutate(filename = filename,
           # response_mode_exposure_phase = sprintf(
           #   "%s \n %s", response_mode, exposure_phase),
           year = ifelse(grepl("submitted", study_ID), Inf,
                         stringr::str_extract(study_ID, "([:digit:]{4})"))
    )
}

avg_month <- 365.2425 / 12.0

all_data <- cached_data %>%
  map_df(load_dataset) %>%
  mutate(all_mod = "",
         mean_age_months = mean_age / avg_month)

studies <- all_data %>%
  group_by(dataset) %>%
  summarise(num_experiments = n(),
            num_papers = length(unique(study_ID)))

subjects <- all_data %>%
  rowwise() %>%
  mutate(n_total = sum(c(n_1, n_2), na.rm = TRUE)) %>%
  distinct(dataset, study_ID, same_infant, .keep_all = TRUE) %>%
  group_by(dataset) %>%
  summarise(num_subjects = sum(n_total))

datasets <- datasets %>%
  rename(dataset = name) %>%
  left_join(studies) %>%
  left_join(subjects) %>%
  rename(name = dataset)

all_data <- all_data %>%
  filter(!is.na(d_calc))

