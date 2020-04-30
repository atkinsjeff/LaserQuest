# uitilities script that brings in needed functions common to all sub-repos

read_csv_file <- function(...) {
  weak_as_tibble(
    read.csv(
      system.file("extdata", ..., mustWork = TRUE),
      stringsAsFactors = FALSE
    )
  )
}
