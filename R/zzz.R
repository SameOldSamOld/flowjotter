#' Add ability to find flowjotter image
#'
#' Quick fix found at
#' https://stackoverflow.com/questions/38791613/including-an-image-in-a-shiny-app-package
.onAttach <- function(libname, pkgname) {
  shiny::addResourcePath('logos',
                         system.file('logos',
                                     package = 'flowjotter'))
}
