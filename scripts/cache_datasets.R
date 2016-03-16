suppressMessages(suppressWarnings({
  library(jsonlite)
  library(RCurl)
  library(dplyr)
  library(yaml)
  library(purrr)
  library(lazyeval)
  library(Hmisc)
}))

source("scripts/compute_es.R")
datasets <- fromJSON(txt = "metadata/datasets.json")
fields <- yaml.load_file("metadata/spec.yaml")


# Validate dataset's values for a given field
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


# Validate dataset's values for all fields
validate_dataset <- function(dataset_meta, dataset_contents) {
  valid_fields <- map(fields, function(field) {
    validate_dataset_field(dataset_meta$name, dataset_contents, field)
  })
  valid_dataset <- all(unlist(valid_fields))
  return(valid_dataset)
}


# Get a dataset's contents from the google sheets
fetch_dataset <- function(dataset_meta) {

  dataset_url <- sprintf(
    "https://docs.google.com/spreadsheets/d/%s/export?id=%s&format=csv",
    dataset_meta$key, dataset_meta$key
  )

  tryCatch({
    dataset_contents <- read.csv(textConnection(getURL(dataset_url)),
                                 stringsAsFactors = FALSE)
    return(dataset_contents)
  },
  error = function(e) {
    cat(sprintf("Can't load dataset '%s' with key '%s'.\n", dataset_meta$name,
                dataset_meta$key))
    return(NULL)
  })

}


# Manipulate a dataset's contents to prepare it for saving
tidy_dataset <- function(dataset_meta, dataset_contents) {

  # # Coerce each field's values to the field's type
  # for (field in fields) {
  #   if (field$field %in% names(dataset_contents)) {
  #     if (field$type == "string") {
  #       dots = list(interp(~as.character(var), var = as.name(field$field)))
  #       dataset_contents <- dataset_contents %>%
  #         mutate_(.dots = setNames(dots, field$field))
  #     } else if (field$type == "numeric") {
  #       dots = list(interp(~as.numeric(var), var = as.name(field$field)))
  #       dataset_contents <- dataset_contents %>%
  #         mutate_(.dots = setNames(dots, field$field))
  #     }
  #   }
  # }

  # Impute values for missing correlations
  set.seed(111)
  if (all(is.na(dataset_contents$corr))) {
    dataset_contents$corr_imputed <- NA
  } else {
    dataset_contents$corr_imputed <- as.numeric(impute(dataset_contents$corr, fun = "random"))
  }

  # Compute effect sizes and variances
  dataset_data <- dataset_contents %>%
    mutate(r = NA, SD_dif = NA) %>%  #TODO: deal with these fields
    mutate(num = row_number()) %>%
    split(.$num) %>%
    map_df(~bind_cols(
      .x, compute_es(.x$participant_design, .x$x_1, .x$x_2, .x$x_dif, .x$SD_1,
                     .x$SD_2, .x$SD_dif, .x$n_1, .x$n_2, .x$t, .x$F, .x$d,
                     .x$d_var, .x$corr, .x$corr_imputed, .x$r, .x$unique_ID,
                     .x$expt_num, .x$special_cases_measures, .x$contrast_sampa))) %>%
    select(-num)

  # Add any other derived values
  method_options <- keep(fields, ~.x$field == "method")[[1]]$options
  method_names <- unlist(map(method_options, ~.x[[names(.x)]]$fullname))
  names(method_names) <- unlist(map(method_options, names))

  dataset_data %>%
    mutate(dataset = dataset_meta[["name"]],
           short_name = dataset_meta[["short_name"]],
           method = unlist(method_names[method])) %>%
    rowwise() %>%
    mutate(mean_age = weighted.mean(c(mean_age_1, mean_age_2), c(n_1, n_2),
                                    na.rm = TRUE),
           n = mean(c(n_1, n_2), na.rm = TRUE))

}


# Save a dataset's contents to a csv file
save_dataset <- function(dataset_meta, dataset_data) {
  write.csv(dataset_data, dataset_meta$file, row.names = FALSE)
  cat(sprintf("Dataset '%s' saved successfully.\n", dataset_meta$name))
}


# Fetch a dataset from google sheets, run it through field validation,
# perform any necessary manipulations of its contents, save it to a file
load_dataset <- function(dataset_short_name) {

  dataset_meta <- datasets %>% filter(short_name == dataset_short_name)
  if (!nrow(dataset_meta)) {
    cat(sprintf("Dataset '%s' isn't in datasets metadata.\n", dataset_short_name))
    return()
  }

  dataset_contents <- fetch_dataset(dataset_meta)
  if (is.null(dataset_contents)) {
    return()
  }

  valid_dataset <- validate_dataset(dataset_meta, dataset_contents)
  if (!valid_dataset) {
    cat(sprintf(
      "Dataset '%s' had one or more validation issues, not being cached.\n",
      dataset_meta$name))
    return()
  }

  dataset_data <- tidy_dataset(dataset_meta, dataset_contents)
  save_dataset(dataset_meta, dataset_data)

}


# Given no arguments, load all datasets
# Given one argument, load the dataset with the given name
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  for (short_name in datasets$short_name) {
    load_dataset(short_name)
  }
} else if (length(args) == 1) {
  if (args %in% datasets$short_name) {
    load_dataset(args)
  } else {
    cat("Please provide a valid dataset name.\n")
  }
} else {
  cat("Usage: Rscript scripts/cache_datasets.R [short_name]\n")
}
