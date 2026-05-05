# ==============================================================================
# app.R — ShinyLabelR  (single-file entry point for shinyapps.io)
# ==============================================================================
# WHY single file: shinyapps.io is reliable only when app.R does ALL the work.
# global.R placed in inst/app/ is not auto-loaded by shinyapps.io.
# options(shiny.autoload.r=FALSE) prevents R/ from being sourced alphabetically
# (which would load auth.R before db.R, crashing the app).
# This file sources R/ modules in the correct explicit dependency order.
# ==============================================================================

options(shiny.autoload.r  = FALSE)    # prevent alphabetical auto-source of R/
options(shiny.maxRequestSize = 50 * 1024^2)  # 50 MB upload limit

# ── 1. Install any missing packages ───────────────────────────────────────────
required_pkgs <- c(
  "shiny", "bslib", "shinyjs",
  "DBI", "RSQLite",
  "jsonlite", "zip",
  "ggplot2", "DT",
  "magick", "base64enc",
  "fs", "tools",
  "httr2", "digest"
)
missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  message("[ShinyLabel] Installing: ", paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs, repos = "https://cran.rstudio.com/",
                   quiet = TRUE, dependencies = TRUE)
}

# ── 2. Load packages ───────────────────────────────────────────────────────────
suppressPackageStartupMessages({
  library(shiny);   library(bslib);    library(shinyjs)
  library(DBI);     library(RSQLite);  library(jsonlite)
  library(ggplot2); library(DT);       library(digest)
})

# ── 3. Find repo root reliably ─────────────────────────────────────────────────
# Works on shinyapps.io (getwd() = deployed app root) and locally.
repo_root <- normalizePath(getwd(), mustWork = TRUE)
cat("[ShinyLabel] repo_root:", repo_root, "\n")
cat("[ShinyLabel] R/ exists:", dir.exists(file.path(repo_root, "R")), "\n")

# ── 4. Source modules in strict dependency order ───────────────────────────────
source_order <- c("db.R", "auth.R", "image_utils.R", "export.R",
                  "run.R", "ui.R", "server.R")
r_dir <- file.path(repo_root, "R")

for (fname in source_order) {
  fpath <- file.path(r_dir, fname)
  if (!file.exists(fpath)) stop("[ShinyLabel] Missing file: ", fpath)
  source(fpath, local = FALSE)
  cat("[ShinyLabel] Loaded:", fname, "\n")
}

# ── 5. Static assets ───────────────────────────────────────────────────────────
www_dir <- file.path(repo_root, "inst", "app", "www")
shiny::addResourcePath("css", file.path(www_dir, "css"))
shiny::addResourcePath("js",  file.path(www_dir, "js"))

exports_dir <- file.path(tempdir(), "sl_exports")
dir.create(exports_dir, showWarnings = FALSE, recursive = TRUE)
shiny::addResourcePath("exports", exports_dir)

# ── 6. Database ────────────────────────────────────────────────────────────────
DB_PATH <- file.path(tempdir(), "shinylabel.db")
cat("[ShinyLabel] DB path:", DB_PATH, "\n")

# ── 7. Launch ──────────────────────────────────────────────────────────────────
shiny::shinyApp(
  ui     = sl_ui(),
  server = sl_server(db_path = DB_PATH)
)
