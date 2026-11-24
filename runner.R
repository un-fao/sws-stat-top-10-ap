# Start the Top10 AP Shiny app on a server.
# Run from the repo folder:  Rscript runner.R
# (.Rprofile activates renv automatically.)
#
# Loads global.R / ui.R / server.R explicitly, because the folder also
# contains RStudio's template app.R, which runApp(".") would pick instead.

options(shiny.host = "0.0.0.0", shiny.port = 8080)

source("global.R", local = globalenv())
ui     <- source("ui.R",     local = globalenv())$value
server <- source("server.R", local = globalenv())$value

shiny::runApp(shiny::shinyApp(ui = ui, server = server),
              host = "0.0.0.0", port = 8080, launch.browser = FALSE)
