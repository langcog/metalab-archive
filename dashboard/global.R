library(jsonlite)
library(dplyr)
library(readr)

includeRmd <- function(path, shiny_data) {
  shiny:::dependsOnFile(path)
  contents = paste(readLines(path, warn = FALSE), collapse = '\n')
  html <- knitr::knit2html(text = contents, fragment.only = TRUE)
  Encoding(html) <- 'UTF-8'
  return(HTML(html))
}

cached_data <- unlist(lapply(list.files('../data/'),
                             function(filename) paste0("data/", filename)))
datasets <- fromJSON(txt = "../datasets.json") %>%
  filter(filename %in% cached_data)

load_dataset <- function(filename) {
  print(paste0("../", filename))
  read.csv(paste0("../", filename), stringsAsFactors = FALSE) %>%
    mutate(#study_num = as.character(study_num),
           filename = filename)
}

avg_month <- 365.2425/12.0

all_data <- bind_rows(lapply(cached_data, load_dataset)) %>%
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
  filter(!is.na(d))
