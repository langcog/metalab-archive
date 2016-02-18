library(jsonlite)
library(dplyr)
library(readr)
library(yaml)
library(lazyeval)
library(purrr)

fields <- yaml.load_file("../spec.yaml")

includeRmd <- function(path, shiny_data) {
  shiny:::dependsOnFile(path)
  contents = paste(readLines(path, warn = FALSE), collapse = '\n')
  html <- knitr::knit2html(text = contents, fragment.only = TRUE)
  Encoding(html) <- 'UTF-8'
  return(HTML(html))
}

cached_data <- list.files('../data/') %>% map(~paste0("data/", .x)) %>% unlist()

datasets <- fromJSON(txt = "../datasets.json") %>%
  filter(filename %in% cached_data)

load_dataset <- function(filename) {

  print(paste0("../", filename))
  dataset_contents <- read.csv(paste0("../", filename), stringsAsFactors = FALSE) %>%
    mutate(filename = filename,
           response_mode_exposure_phase = sprintf("%s: %s", response_mode, exposure_phase))

  # Coerce each field's values to the field's type
  for (field in fields) {
    if (field$field %in% names(dataset_contents)) {
      if (field$type == "string") {
        dots = list(interp(~as.character(var), var = as.name(field$field)))
        dataset_contents <- dataset_contents %>%
          mutate_(.dots = setNames(dots, field$field))
      } else if (field$type == "numeric") {
        dots = list(interp(~as.numeric(var), var = as.name(field$field)))
        dataset_contents <- dataset_contents %>%
          mutate_(.dots = setNames(dots, field$field))
      }
    }
  }

  dataset_contents
}

avg_month <- 365.2425/12.0

all_data <- cached_data %>%
  map_df(load_dataset) %>%
  mutate(all_mod = "All",
         mean_age_months = mean_age / avg_month)

studies <- all_data %>%
  group_by(filename) %>%
  summarise(num_experiments = n(),
            num_papers = length(unique(unique_ID)))

subjects <- all_data %>%
  rowwise() %>%
  mutate(n_total = sum(c(n_1, n_2), na.rm = TRUE)) %>%
  group_by(filename) %>%
  summarise(num_subjects = sum(n_total))

datasets <- datasets %>%
  left_join(studies) %>%
  left_join(subjects)

all_data <- all_data %>%
  filter(!is.na(d_calc))
