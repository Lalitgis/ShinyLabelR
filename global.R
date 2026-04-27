# global.R — runs once when shinyapps.io starts the app
# Ensures all required packages are available

required_pkgs <- c(
  "shiny", "bslib", "shinyjs",
  "DBI", "RSQLite", "jsonlite",
  "ggplot2", "DT",
  "magick", "base64enc",
  "zip", "fs", "tools"
)

missing <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(missing) > 0) {
  install.packages(missing, repos = "https://cran.rstudio.com/")
}
