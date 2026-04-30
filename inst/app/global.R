# ==============================================================================
# global.R — ShinyLabelR
#
# Runs ONCE before app.R on every startup — locally and on shinyapps.io.
# Handles: upload size limit, package loading, R module sourcing.
# ==============================================================================


# ── 0. Upload size limit ───────────────────────────────────────────────────────
options(shiny.maxRequestSize = 50 * 1024^2)   # 50 MB per uploaded file


# ── 1. Load packages ───────────────────────────────────────────────────────────
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
  library(httr2)
})


# ── 2. Detect environment ──────────────────────────────────────────────────────
on_shinyapps <- nzchar(Sys.getenv("SHINYAPPS_TOKEN")) ||
                nzchar(Sys.getenv("SHINY_HOST"))

message("[ShinyLabelR] Running on shinyapps.io: ", on_shinyapps)


# ── 3. Find the R/ folder ─────────────────────────────────────────────────────
# The challenge: on shinyapps.io the working directory is the repo root
# (/srv/connect/apps/ShinyLabelR) but global.R lives inside inst/app/.
# We try four candidate locations in order of preference and use the first
# one that actually exists and contains .R files.

candidates <- c(
  # 1. Repo root R/ — correct on shinyapps.io
  file.path(getwd(), "R"),

  # 2. Two levels up from inst/app/ — also correct on shinyapps.io
  normalizePath(file.path(dirname(sys.frame(1)$ofile), "..", "..", "R"),
                mustWork = FALSE),

  # 3. Next to app.R when running locally via shiny::runApp("inst/app")
  normalizePath(file.path(dirname(sys.frame(1)$ofile), "R"),
                mustWork = FALSE),

  # 4. Absolute shinyapps.io path — hard fallback
  "/srv/connect/apps/ShinyLabelR/R"
)

r_dir <- NULL
for (candidate in candidates) {
  if (dir.exists(candidate) &&
      length(list.files(candidate, pattern = "\\.R$")) > 0) {
    r_dir <- normalizePath(candidate, mustWork = FALSE)
    break
  }
}

if (is.null(r_dir)) {
  stop(
    "[ShinyLabelR] Cannot find the R/ folder. Tried:\n",
    paste0("  - ", candidates, collapse = "\n"),
    "\nWorking directory: ", getwd()
  )
}

message("[ShinyLabelR] Found R/ at: ", r_dir)


# ── 4. Source all R modules ────────────────────────────────────────────────────
# Source in explicit order so dependencies are met:
#   db.R          must come before server.R (defines sl_init_db etc.)
#   image_utils.R must come before server.R (defines sl_image_b64 etc.)
#   export.R      must come before server.R (defines sl_export_yolo etc.)
#   ui.R          must come before app.R    (defines sl_ui)
#   server.R      last — depends on all of the above
source_order <- c("db.R", "image_utils.R", "export.R", "run.R", "ui.R", "server.R")

for (fname in source_order) {
  fpath <- file.path(r_dir, fname)
  if (file.exists(fpath)) {
    source(fpath, local = FALSE)
    message("[ShinyLabelR] Loaded: ", fname)
  } else {
    warning("[ShinyLabelR] File not found, skipping: ", fpath)
  }
}

message("[ShinyLabelR] All modules loaded. Upload limit: 50MB per file.")
