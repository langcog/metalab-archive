reports <- yaml::yaml.load_file("../metadata/reports.yaml")
purrr::walk(reports, ~rmarkdown::render(sprintf("../reports/%s.Rmd", .x$file),
                                        output_dir = "../reports/out"))
