# ==============================================================================
# global.R — ShinyLabelR
#
# Runs ONCE before app.R on every startup — locally and on shinyapps.io.
# Handles: upload size limit, package installation, package loading,
#          R module sourcing, and environment detection.
# ==============================================================================


# ── 0. Upload size limit ───────────────────────────────────────────────────────
# Shiny's default is only 5MB — far too small for high-res images.
# Raised to 50MB per file. Increase further for satellite/medical imagery.
# Must be set BEFORE the app starts (here in global.R is the right place).
options(shiny.maxRequestSize = 50 * 1024^2)   # 50 MB per uploaded file


# ── 1. Required packages ───────────────────────────────────────────────────────
required_pkgs <- c(
  # Shiny framework
  "shiny",
  "bslib",
  "shinyjs",

  # Database
  "DBI",
  "RSQLite",

  # Data / export
  "jsonlite",
  "zip",

  # Plots and tables
  "ggplot2",
  "DT",

  # Image handling
  "magick",
  "base64enc",

  # File utilities
  "fs",
  "tools",
  "utils"
)


# ── 2. Install any missing packages ───────────────────────────────────────────
missing_pkgs <- required_pkgs[
  !sapply(required_pkgs, requireNamespace, quietly = TRUE)
]

if (length(missing_pkgs) > 0) {
  message("[ShinyLabelR] Installing missing packages: ",
          paste(missing_pkgs, collapse = ", "))
  install.packages(
    missing_pkgs,
    repos        = "https://cran.rstudio.com/",
    quiet        = TRUE,
    dependencies = TRUE
  )
  message("[ShinyLabelR] Package installation complete.")
}


# ── 3. Load packages ───────────────────────────────────────────────────────────
suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(shinyjs)
  library(DBI)
  library(RSQLite)
  library(jsonlite)
  library(ggplot2)
  library(DT)
})


# ── 4. Detect environment ──────────────────────────────────────────────────────
on_shinyapps <- nzchar(Sys.getenv("SHINYAPPS_TOKEN")) ||
                nzchar(Sys.getenv("SHINY_HOST"))

message("[ShinyLabelR] Running on shinyapps.io: ", on_shinyapps)


# ── 5. Resolve app directory ───────────────────────────────────────────────────
# Works with shiny::runApp(), shiny::runGitHub(), and shinyapps.io deployment
app_dir <- tryCatch(
  normalizePath(dirname(sys.frame(1)$ofile), mustWork = FALSE),
  error = function(e) NULL
)
if (is.null(app_dir) || !nzchar(app_dir)) {
  app_dir <- normalizePath(getwd(), mustWork = TRUE)
}
message("[ShinyLabelR] App directory: ", app_dir)


# ── 6. Source all R modules ────────────────────────────────────────────────────
# Sources every .R file in the R/ folder in alphabetical order:
#   db.R            → database init and CRUD
#   export.R        → YOLO and COCO export functions
#   image_utils.R   → base64 encoding, magick helpers, URL fetch
#   run.R           → run_shinylabel() launcher
#   server.R        → Shiny server logic
#   ui.R            → Shiny UI definition
# Source in explicit dependency order:
# db.R must load before auth.R (auth uses sl_create_token etc.)
# auth.R must load before server.R (server uses auth_send_* functions)
source_order <- c("db.R", "auth.R", "image_utils.R", "export.R", "run.R", "ui.R", "server.R")

for (fname in source_order) {
  fpath <- file.path(app_dir, "R", fname)
  if (file.exists(fpath)) {
    source(fpath, local = FALSE)
    message("[ShinyLabelR] Loaded: ", fname)
  } else {
    warning("[ShinyLabelR] File not found, skipping: ", fpath)
  }
}

message("[ShinyLabelR] All modules loaded. Upload limit: 50MB per file.")
