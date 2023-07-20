#' Flow Jotter Shiny App
#'
#' Use this wrapper function to run the shiny app in your RStudio Viewer, or favourite browser. No data needs to be provided to flowjotter until the shiny app is live, and data is stored locally on your machine temporarily.
#'
#' @examples
#' \dontrun{flowjotter()}
#' @export
flowjotter <- function() {
  shiny::shinyApp(ui, flowjotterServer)
}
