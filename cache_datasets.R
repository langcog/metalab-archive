suppressMessages(suppressWarnings({
  library(jsonlite)
  library(RCurl)
  library(dplyr)
}))

datasets <- fromJSON(txt = "datasets.json")
mapping <- fromJSON(txt = "mapping.json")

required_cols <- c("unique_ID", "long_cite", "short_cite", "contributor", "study_num", "condition",
                   "response_mode", "procedure", "method", "dependent_measure", "native_lang", "infant_type",
                   "d", "d_var", "mean_age_1", "mean_age_2", "n_1", "n_2")

valid_columns <- function(dataset_name, dataset_contents) {
  missing_cols <- required_cols[!(required_cols %in% names(dataset_contents))]
  if (length(missing_cols)) {
    cat(paste(
      sprintf("Dataset '%s' is missing required column(s):", dataset_name),
      paste(missing_cols, collapse = ", "),
      "\n"
    ))
    return(FALSE)
  }
  return(TRUE)
}

valid_values <- function(dataset_name, dataset_contents, column, mapping_values) {
  if (column %in% names(dataset_contents)) {
    invalid_procedures <- unique(dataset_contents[[column]][!(dataset_contents[[column]] %in% mapping_values)])
    if (length(invalid_procedures)) {
      cat(paste(
        sprintf("Dataset '%s' has invalid value(s) for '%s':", dataset_name, column),
        paste(invalid_procedures, collapse = ", "),
        "\n"
      ))
      return(FALSE)
    }
  }
  return(TRUE)
}

get_dataset <- function(dataset_meta) {
  
  cat(sprintf("\nLoading dataset '%s'\n", dataset_meta$name))
  
  dataset_url <- sprintf("https://docs.google.com/spreadsheets/d/%s/export?id=%s&format=csv",
                         dataset_meta$key, dataset_meta$key)
  
  tryCatch(dataset_contents <- read.csv(textConnection(getURL(dataset_url)), stringsAsFactors = FALSE),
           error = function(e) cat(sprintf("Can't load dataset %s with key %s\n", dataset_meta$name, dataset_meta$key)))
  
  if (exists("dataset_contents")) {
    
    valid_dataset <- all(
      valid_columns(dataset_meta$name, dataset_contents),
      valid_values(dataset_meta$name, dataset_contents, "response_mode", mapping$response_mode),
      valid_values(dataset_meta$name, dataset_contents, "procedure", mapping$procedure),
      valid_values(dataset_meta$name, dataset_contents, "method", names(mapping$method))
    )
    
    if (valid_dataset) {
      
      dataset_contents %>%
        mutate(dataset = dataset_meta[["name"]],
               study_num = as.character(study_num),
               method = unlist(mapping$method[method]),
               response_mode_procedure = paste(response_mode, procedure, sep = ": "),
               d = d, #TODO: calculate effect size
               d_var = d_var # TODO: calculate effect size variance
        ) %>%
        rowwise() %>%
        mutate(mean_age = weighted.mean(c(mean_age_1, mean_age_2), c(n_1, n_2),
                                        na.rm = TRUE),
               n = mean(c(n_1, n_2), na.rm = TRUE)) %>%
        filter(!is.na(d))
    }
    
  }
  
}

for (i in 1:nrow(datasets)) {
  dataset_meta <- datasets[i,]
  dataset_data <- get_dataset(dataset_meta)
  if(!is.null(dataset_data)) {
    write.csv(dataset_data, dataset_meta$file, row.names = FALSE)
  }
}