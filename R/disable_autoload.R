# disable_autoload.R
# Presence of this file tells Shiny NOT to auto-source R/ alphabetically.
# The actual option is also set in app.R for belt-and-suspenders reliability.
options(shiny.autoload.r = FALSE)
