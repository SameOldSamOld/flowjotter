# Quick fix found at
# https://stackoverflow.com/questions/38791613/including-an-image-in-a-shiny-app-package
# for including app image and
.onAttach <- function(libname, pkgname) {
  shiny::addResourcePath('logos',
                         system.file('logos',
                                     package = 'flowjotter'))
}
