# ==============================================================================
# app.R — ShinyLabelR
# ==============================================================================
# The warning "Loading R/ subdirectory" means shinyapps.io auto-sources R/
# alphabetically BEFORE this file runs — auth.R loads before db.R = crash.
# Solution: Do NOT use R/ subdirectory at all for deployment.
# All module code is sourced explicitly here from their paths.
# The disable_autoload.R file suppresses this, but as a belt-and-suspenders
# measure we also set the option at the very top.
# ==============================================================================

local({
  # Must be set before anything else — prevents R/ auto-load
  options(shiny.autoload.r = FALSE)
})

options(shiny.maxRequestSize = 50 * 1024^2)

# ── 1. Packages ────────────────────────────────────────────────────────────────
suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(shinyjs)
  library(DBI)
  library(RSQLite)
  library(jsonlite)
  library(ggplot2)
  library(DT)
  library(digest)
})

# ── 2. Source modules in correct dependency order ──────────────────────────────
# We source from R/ relative to the app root (getwd() on shinyapps.io)
app_root <- normalizePath(getwd(), mustWork = TRUE)
r_dir    <- file.path(app_root, "R")

cat("[ShinyLabel] app_root:", app_root, "\n")
cat("[ShinyLabel] R/ exists:", dir.exists(r_dir), "\n")

for (fname in c("db.R", "auth.R", "image_utils.R", "export.R", "run.R", "ui.R", "server.R")) {
  fpath <- file.path(r_dir, fname)
  if (!file.exists(fpath)) stop("[ShinyLabel] Missing required file: ", fpath)
  source(fpath, local = FALSE)
  cat("[ShinyLabel] Sourced:", fname, "\n")
}

# ── 3. Static assets ───────────────────────────────────────────────────────────
www_dir <- file.path(app_root, "inst", "app", "www")
addResourcePath("css",     file.path(www_dir, "css"))
addResourcePath("js",      file.path(www_dir, "js"))
addResourcePath("exports", file.path(tempdir(), "sl_exports"))
dir.create(file.path(tempdir(), "sl_exports"), showWarnings = FALSE, recursive = TRUE)

# ── 4. Persistent database path ────────────────────────────────────────────────
# /srv/connect/apps/ShinyLabelR persists across shinyapps.io restarts.
# getwd() works for local development.
# tempdir() is last resort only (data lost on restart).
DB_PATH <- {
  candidates <- c(
    file.path("/srv/connect/apps/ShinyLabelR", "shinylabel.db"),
    file.path(app_root, "shinylabel.db"),
    file.path(tempdir(), "shinylabel.db")
  )
  chosen <- candidates[1]  # default
  for (p in candidates) {
    d <- dirname(p)
    if (dir.exists(d) && file.access(d, 2L) == 0L) {
      chosen <- p
      break
    }
  }
  cat("[ShinyLabel] DB path:", chosen, "\n")
  chosen
}

# ── 5. Run ─────────────────────────────────────────────────────────────────────
shinyApp(
  ui     = sl_ui(),
  server = sl_server(db_path = DB_PATH)
)
