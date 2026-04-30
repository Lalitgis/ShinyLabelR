# ==============================================================================
# app.R — ShinyLabelR root entry point
# ==============================================================================

options(shiny.maxRequestSize = 50 * 1024^2)

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(shinyjs)
  library(DBI)
  library(RSQLite)
  library(jsonlite)
  library(ggplot2)
  library(DT)
  library(magick)
  library(base64enc)
  library(zip)
  library(fs)
  library(tools)
})

# ── Repo root — use getwd() directly, it is always correct on shinyapps.io ────
repo_root <- getwd()
cat("[ShinyLabel] Working directory:", repo_root, "\n")
cat("[ShinyLabel] DESCRIPTION exists:", file.exists(file.path(repo_root, "DESCRIPTION")), "\n")
cat("[ShinyLabel] R/ exists:", dir.exists(file.path(repo_root, "R")), "\n")

# ── Source R modules ───────────────────────────────────────────────────────────
r_dir <- file.path(repo_root, "R")
for (fname in c("db.R", "image_utils.R", "export.R", "run.R", "ui.R", "server.R")) {
  fpath <- file.path(r_dir, fname)
  if (file.exists(fpath)) {
    source(fpath, local = FALSE)
    cat("[ShinyLabel] Loaded:", fname, "\n")
  } else {
    stop("[ShinyLabel] Cannot find: ", fpath)
  }
}

# ── Static assets ──────────────────────────────────────────────────────────────
www_dir <- file.path(repo_root, "inst", "app", "www")
shiny::addResourcePath("css",  file.path(www_dir, "css"))
shiny::addResourcePath("js",   file.path(www_dir, "js"))

exports_dir <- file.path(tempdir(), "sl_exports")
dir.create(exports_dir, showWarnings = FALSE, recursive = TRUE)
shiny::addResourcePath("exports", exports_dir)

# ── Database ───────────────────────────────────────────────────────────────────
DB_PATH <- file.path(tempdir(), "shinylabel.db")
cat("[ShinyLabel] DB path:", DB_PATH, "\n")

# ── Launch ─────────────────────────────────────────────────────────────────────
shiny::shinyApp(
  ui     = sl_ui(),
  server = sl_server(db_path = DB_PATH)
)
