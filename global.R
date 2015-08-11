library(jsonlite)
library(dplyr)

name <- "MetaLab"
cached_data <- unlist(lapply(list.files('data/'), function(filename) paste0("data/", filename)))
datasets <- fromJSON(txt = "datasets.json") %>%
  filter(filename %in% cached_data)

includeRmd <- function(path, shiny_data) {
  shiny:::dependsOnFile(path)
  contents = paste(readLines(path, warn = FALSE), collapse = '\n')
  html <- knitr::knit2html(text = contents, fragment.only = TRUE)
  Encoding(html) <- 'UTF-8'
  return(HTML(html))
}
