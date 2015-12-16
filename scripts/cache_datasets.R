suppressMessages(suppressWarnings({
  library(jsonlite)
  library(RCurl)
  library(dplyr)
  library(yaml)
  library(purrr)
}))

datasets <- fromJSON(txt = "datasets.json")
fields <- yaml.load_file("spec.yaml")

validate_dataset_field <- function(dataset_name, dataset_contents, field) {
  if (field$required) {
    if (field$field %in% names(dataset_contents)) {
      if (field$type == "options") {
        if (class(field$options) == "list") {
          options <- names(unlist(field$options, recursive = FALSE))
        } else {
          options <- field$options
        }
        invalid_values <- setdiff(unique(dataset_contents[[field$field]]), options)
        for (value in invalid_values) {
          cat(sprintf("Dataset '%s' has invalid value '%s' for field '%s'.\n",
                      dataset_name, value, field$field))
        }
        if (length(invalid_values)) {
          return(FALSE)
        }
      }
    } else {
      cat(sprintf("Dataset '%s' is missing required field: '%s'.\n",
                  dataset_name, field$field))
      return(FALSE)
    }
  }
  return(TRUE)
}

# Fetches a dataset from Google Docs and runs it through valid_columns and valid_values
# for response_mode, procedure, and method.
load_dataset <- function(dataset_short_name) {

  dataset_meta <- datasets %>% filter(short_name == dataset_short_name)
  dataset_url <- sprintf("https://docs.google.com/spreadsheets/d/%s/export?id=%s&format=csv",
                         dataset_meta$key, dataset_meta$key)

  tryCatch(dataset_contents <- read.csv(textConnection(getURL(dataset_url)),
                                        stringsAsFactors = FALSE),
           error = function(e) cat(sprintf("Can't load dataset %s with key %s.\n",
                                           dataset_meta$name, dataset_meta$key)))

  if (exists("dataset_contents")) {

    valid_fields <- map(fields, function(field) {
      validate_dataset_field(dataset_meta$name, dataset_contents, field)
    })
    valid_dataset <- all(unlist(valid_fields))

    if (valid_dataset) {

      dataset_data <- dataset_contents %>%
        mutate(dataset = dataset_meta[["name"]]
               #study_num = as.character(study_num),
               #method = unlist(mapping$method[method]),
               #response_mode_procedure = paste(response_mode, procedure, sep = ": "),
               #d = d, #TODO: calculate effect size
               #d_var = d_var # TODO: calculate effect size variance
        ) %>%
        rowwise() %>%
        mutate(mean_age = weighted.mean(c(mean_age_1, mean_age_2), c(n_1, n_2),
                                        na.rm = TRUE),
               n = mean(c(n_1, n_2), na.rm = TRUE))
      cat(sprintf("Dataset '%s' validated successfully.\n", dataset_meta$name))
      write.csv(dataset_data, dataset_meta$file, row.names = FALSE)
    } else {
      cat(sprintf("Dataset '%s' had one or more validation issues, not being cached.\n", dataset_meta$name))
    }
  }
}

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  for (short_name in datasets$short_name) {
    load_dataset(short_name)
  }
} else if (length(args) == 1) {
  load_dataset(args)
} else {
  cat("Usage: Rscript scripts/cache_datasets.R [short_name]\n")
}
