#' Launch ShinyLabel
#'
#' Starts the ShinyLabel annotation tool as a Shiny application.
#'
#' @param db_path   Path to the SQLite database file. Created if it doesn't exist.
#'                  Defaults to "shinylabel.db" in the current working directory.
#'                  For team use, point all annotators to the same file on a
#'                  shared network drive.
#' @param host      Host to run on. Use "0.0.0.0" to expose on LAN for team access.
#' @param port      Port number. NULL = auto-assign.
#' @param launch.browser Whether to open browser automatically.
#'
#' @examples
#' \dontrun{
#'   # Solo use
#'   run_shinylabel()
#'
#'   # Team use (shared network path)
#'   run_shinylabel(db_path = "//server/shared/project/annotations.db",
#'                  host = "0.0.0.0", port = 3838)
#' }
#' @export
run_shinylabel <- function(
    db_path        = "shinylabel.db",
    host           = "127.0.0.1",
    port           = NULL,
    launch.browser = TRUE
) {
  # Add www/ folder to resource path so CSS/JS are served.
  #
  # When installed: inst/app/www is placed at <pkg_root>/app/www by R's
  # installation process.  system.file("app", "www") finds it correctly in
  # both fully-installed and devtools::load_all() scenarios.
  #
  # The old code used system.file("www") which looked for <pkg_root>/www —
  # a path that does not exist — and always returned "".
  www_dir <- system.file("app", "www", package = "shinylabel")

  if (!nzchar(www_dir) || !dir.exists(www_dir)) {
    # Last-resort: running directly from the source tree (e.g. shiny::runApp())
    www_dir <- file.path(
      normalizePath(system.file(package = "shinylabel"), mustWork = FALSE),
      "app", "www")
  }

  if (!dir.exists(www_dir))
    stop(
      "[shinylabel] Cannot locate the www/ directory at: ", www_dir, "\n",
      "Try reinstalling with:  devtools::install('.')"
    )

  shiny::addResourcePath("css", file.path(www_dir, "css"))
  shiny::addResourcePath("js",  file.path(www_dir, "js"))
  shiny::addResourcePath("img", file.path(www_dir, "img"))

  app <- shiny::shinyApp(
    ui     = sl_ui(),
    server = sl_server(db_path = db_path)
  )

  shiny::runApp(
    app,
    host           = host,
    port           = port,
    launch.browser = launch.browser
  )
}
