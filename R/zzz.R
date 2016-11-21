.onLoad <- function(libname, pkgname) {
  shiny::registerInputHandler('nodeOut', function(data, ...) {
    unlist(data)
  })
}
