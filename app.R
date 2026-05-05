# app.R — ShinyLabelR
# Single entry point for shinyapps.io.
# CRITICAL: shinyApp() must be the last expression — its value IS the return value.
# options() must be set before any sourcing happens.

options(shiny.autoload.r = FALSE)   # must be line 1 logic — before ANY sourcing
options(shiny.maxRequestSize = 50 * 1024^2)

suppressPackageStartupMessages({
  library(shiny);   library(bslib);    library(shinyjs)
  library(DBI);     library(RSQLite);  library(jsonlite)
  library(ggplot2); library(DT);       library(digest)
})

# Source modules in strict dependency order
app_root <- normalizePath(getwd(), mustWork = TRUE)
r_dir    <- file.path(app_root, "R")
cat("[ShinyLabel] Starting from:", app_root, "\n")

for (fname in c("db.R","auth.R","image_utils.R","export.R","run.R","ui.R","server.R")) {
  fpath <- file.path(r_dir, fname)
  if (!file.exists(fpath)) stop("[ShinyLabel] Missing: ", fpath)
  source(fpath, local = FALSE)
  cat("[ShinyLabel] Loaded:", fname, "\n")
}

# Static assets
www_dir <- file.path(app_root, "inst", "app", "www")
addResourcePath("css", file.path(www_dir, "css"))
addResourcePath("js",  file.path(www_dir, "js"))
exports_dir <- file.path(tempdir(), "sl_exports")
dir.create(exports_dir, showWarnings = FALSE, recursive = TRUE)
addResourcePath("exports", exports_dir)

# Persistent DB — try locations in order of preference
DB_PATH <- local({
  candidates <- c(
    file.path("/srv/connect/apps/ShinyLabelR", "shinylabel.db"),
    file.path(app_root, "shinylabel.db"),
    file.path(tempdir(), "shinylabel.db")
  )
  chosen <- candidates[length(candidates)]  # fallback
  for (p in candidates) {
    d <- dirname(p)
    if (dir.exists(d) && file.access(d, 2L) == 0L) { chosen <- p; break }
  }
  cat("[ShinyLabel] DB:", chosen, "\n")
  chosen
})

# CRITICAL: shinyApp() must be the bare final expression — no assignment, no cat() after it
shinyApp(
  ui     = sl_ui(),
  server = sl_server(db_path = DB_PATH)
)
