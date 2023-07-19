flowjotter <- function(...) {
  shiny::shinyApp(ui, flowjotterServer, ...)
}
