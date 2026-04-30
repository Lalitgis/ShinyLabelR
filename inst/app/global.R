library(shiny)
library(bslib)
library(shinyjs)
library(DBI)
library(RSQLite)
library(magick)
library(jsonlite)
library(ggplot2)
library(DT)
library(zip)
library(base64enc)
library(fs)
library(httr2)

# Explicitly source each R file — avoids package detection issues on shinyapps.io
app_dir <- "/srv/connect/apps/ShinyLabelR"

for (f in c("R/db.R", "R/image_utils.R", "R/export.R", "R/ui.R", "R/server.R")) {
  source(file.path(app_dir, f), local = FALSE)
}

# Database setup
on_shinyapps <- nzchar(Sys.getenv("SHINYAPPS_TOKEN")) ||
                nzchar(Sys.getenv("SHINY_HOST")) ||
                identical(Sys.getenv("R_CONFIG_ACTIVE"), "shinyapps")

DB_PATH <- if (on_shinyapps) {
  file.path(tempdir(), "shinylabel.db")
} else {
  file.path(app_dir, "inst", "app", "shinylabel.db")
}

con <- sl_init_db(DB_PATH)
message("[ShinyLabel] DB path: ", DB_PATH)
