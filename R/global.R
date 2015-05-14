datasets <- fromJSON(txt = "../datasets.json")

includeRmd <- function(path, datasets) {
  shiny:::dependsOnFile(path)
  contents = paste(readLines(path, warn = FALSE), collapse = '\n')
  html <- knitr::knit2html(text = contents, fragment.only = TRUE)
  Encoding(html) <- 'UTF-8'
  return(HTML(html))
}