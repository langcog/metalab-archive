#library(jsonlite)
library(dplyr)
#library(yaml)
#library(lazyeval)
library(purrr)
#library(stringr)

fields <- yaml::yaml.load_file("../metadata/spec.yaml")
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

  # dataset_contents <- read.csv(paste0("../", filename),
  #                              stringsAsFactors = FALSE) %>%
  file.path("..", "data", filename) %>%
    feather::read_feather() %>%
    mutate(filename = filename,
           response_mode_exposure_phase = sprintf(
             "%s \n %s", response_mode, exposure_phase),
           year = ifelse(grepl("submitted", unique_ID), Inf,
                         stringr::str_extract(unique_ID, "([:digit:]{4})"))
    )

  # # Coerce each field's values to the field's type
  # for (field in fields) {
  #   if (field$field %in% names(dataset_contents)) {
  #     if (field$type == "string") {
  #       dots <- list(interp(~as.character(var), var = as.name(field$field)))
  #       dataset_contents <- dataset_contents %>%
  #         mutate_(.dots = setNames(dots, field$field))
  #     } else if (field$type == "numeric") {
  #       dots <- list(interp(~as.numeric(var), var = as.name(field$field)))
  #       dataset_contents <- dataset_contents %>%
  #         mutate_(.dots = setNames(dots, field$field))
  #     }
  #   }
  # }

  #dataset_contents
}

avg_month <- 365.2425 / 12.0

all_data <- cached_data %>%
  map_df(load_dataset) %>%
  mutate(all_mod = "",
         mean_age_months = mean_age / avg_month)

studies <- all_data %>%
  group_by(dataset) %>%
  summarise(num_experiments = n(),
            num_papers = length(unique(unique_ID)))

subjects <- all_data %>%
  rowwise() %>%
  mutate(n_total = sum(c(n_1, n_2), na.rm = TRUE)) %>%
  distinct(dataset, unique_ID, same_infant) %>%
  group_by(dataset) %>%
  summarise(num_subjects = sum(n_total))

datasets <- datasets %>%
  rename(dataset = name) %>%
  left_join(studies) %>%
  left_join(subjects) %>%
  rename(name = dataset)

all_data <- all_data %>%
  filter(!is.na(d_calc))
