# global.R — ShinyLabelR
# NOTE: All module sourcing and setup is handled in app.R.
# This file intentionally minimal — shinyapps.io may call it
# but app.R is the true entry point and does all the work.
options(shiny.maxRequestSize = 50 * 1024^2)
