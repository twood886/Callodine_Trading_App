get_replacements <- function(portfolio_id) {

  file_name <- paste0(portfolio_id, ".csv")
  file_path <- file.path(
    "app",
    "writeable",
    file_name
  )

  if (!file.exists(file_path)) {
    stop("File does not exist")
  }

  replacements <- read.csv(file_path, stringsAsFactors = FALSE)

}