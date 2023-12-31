#' Flow Jotter Shiny App Wrapper
#'
#' Use this wrapper function to run the shiny app in your RStudio Viewer, or favourite
#'   browser. No data needs to be provided to flowjotter until the shiny app is live.
#'   Uploaded data is stored locally on your machine temporarily.
#'
#' @examples
#' \dontrun{flowjotter()}
#' @export
flowjotter <- function() {
  shiny::shinyApp(flowjotter_ui, flowjotter_server)
}
