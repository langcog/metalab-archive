suppressMessages(suppressWarnings({
  library(jsonlite)
  library(RCurl)
  library(dplyr)
  library(yaml)
  library(purrr)
  library(lazyeval)
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
        if (!is.null(field$nullable) && field$nullable) {
          invalid_values <- na.omit(invalid_values)
        }
        if (length(invalid_values)) {
          for (value in invalid_values) {
            cat(sprintf("Dataset '%s' has invalid value '%s' for field '%s'.\n",
                        dataset_name, value, field$field))
          }
          return(FALSE)
        }
      } else if (field$type == "numeric") {
        field_contents <- dataset_contents[[field$field]]
        if (!(is.numeric(field_contents) || all(is.na(field_contents)))) {
          cat(sprintf("Dataset '%s' has wrong type for numeric field '%s'.\n",
                      dataset_name, field$field))
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

# Fetches a dataset from Google Docs and runs it through field validation.
load_dataset <- function(dataset_short_name) {

  dataset_meta <- datasets %>% filter(short_name == dataset_short_name)
  dataset_url <- sprintf("https://docs.google.com/spreadsheets/d/%s/export?id=%s&format=csv",
                         dataset_meta$key, dataset_meta$key)

  tryCatch({
    dataset_contents <- read.csv(textConnection(getURL(dataset_url)),
                                 stringsAsFactors = FALSE)
  },
  error = function(e) cat(sprintf("Can't load dataset %s with key %s.\n",
                                  dataset_meta$name, dataset_meta$key)))

  if (exists("dataset_contents")) {

    valid_fields <- map(fields, function(field) {
      validate_dataset_field(dataset_meta$name, dataset_contents, field)
    })
    valid_dataset <- all(unlist(valid_fields))

    if (valid_dataset) {

      for (field in fields) {
        if (field$field %in% names(dataset_contents)) {
          if (field$type == "string") {
            dots = list(interp(~as.character(var), var = as.name(field$field)))
          } else if (field$type == "numeric") {
            dots = list(interp(~as.numeric(var), var = as.name(field$field)))
          }
          dataset_contents <- dataset_contents %>%
            mutate_(.dots = setNames(dots, field$field))
        }
      }

      dataset_data <- dataset_contents %>%
        mutate(dataset = dataset_meta[["name"]]
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
      cat(sprintf(
        "Dataset '%s' had one or more validation issues, not being cached.\n",
        dataset_meta$name))
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
