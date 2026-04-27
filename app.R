# ─────────────────────────────────────────────────────────────────
# ShinyLabel — app.R
# Works locally AND on shinyapps.io without any changes.
# ─────────────────────────────────────────────────────────────────

# ── Detect environment ─────────────────────────────────────────────────────
on_shinyapps <- nzchar(Sys.getenv("SHINYAPPS_TOKEN")) ||
                identical(Sys.getenv("R_CONFIG_ACTIVE"), "shinyapps") ||
                nzchar(Sys.getenv("SHINY_HOST"))

# ── Resolve app directory ──────────────────────────────────────────────────
app_dir <- tryCatch(
  normalizePath(dirname(sys.frame(1)$ofile), mustWork = FALSE),
  error = function(e) NULL
)
if (is.null(app_dir) || !nzchar(app_dir)) {
  app_dir <- normalizePath(getwd(), mustWork = TRUE)
}
cat("[ShinyLabel] Running from:", app_dir, "\n")
cat("[ShinyLabel] On shinyapps.io:", on_shinyapps, "\n")

# ── Source all R modules ───────────────────────────────────────────────────
r_files <- list.files(file.path(app_dir, "R"), pattern = "\\.R$", full.names = TRUE)
for (f in r_files) source(f, local = FALSE)

# ── Static assets ──────────────────────────────────────────────────────────
shiny::addResourcePath("css", file.path(app_dir, "www", "css"))
shiny::addResourcePath("js",  file.path(app_dir, "www", "js"))

# Create www/img/ if it doesn't exist (optional folder — avoids crash)
img_dir <- file.path(app_dir, "www", "img")
dir.create(img_dir, showWarnings = FALSE, recursive = TRUE)
shiny::addResourcePath("img", img_dir)

exports_dir <- if (on_shinyapps) {
  # shinyapps.io: use tempdir — writable, survives the session
  file.path(tempdir(), "sl_exports")
} else {
  # Local: use www/exports/ next to app.R
  file.path(app_dir, "www", "exports")
}
dir.create(exports_dir, showWarnings = FALSE, recursive = TRUE)
shiny::addResourcePath("exports", exports_dir)

# ── Database path ──────────────────────────────────────────────────────────
# shinyapps.io: tempdir() — data resets on each restart (acceptable for demo)
# Local / Shiny Server: next to app.R — persistent
DB_PATH <- if (on_shinyapps) {
  file.path(tempdir(), "shinylabel.db")
} else {
  file.path(app_dir, "shinylabel.db")
}
cat("[ShinyLabel] DB path:", DB_PATH, "\n")

# ── Launch ─────────────────────────────────────────────────────────────────
shiny::shinyApp(
  ui     = sl_ui(),
  server = sl_server(db_path = DB_PATH)
)
