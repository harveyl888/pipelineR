pkg.env <- new.env()

.onLoad <- function(libname, pkgname) {
  shiny::registerInputHandler('nodeOut', function(data, ...) {
    unlist(data)
  }, force = TRUE)

  .depth <- function(this) ifelse(is.list(this), 1L + max(sapply(this, .depth)), 0L)  # http://stackoverflow.com/questions/13432863/determine-level-of-nesting-in-r

  shiny::registerInputHandler('linksTable', function(data, ...) {
    if (is.null(data)) return(data.frame())
    if (.depth(data) == 1) {
      return(as.data.frame(data))
    } else {
      return(do.call(rbind.data.frame, data))
    }
  }, force = TRUE)

}

.onUnload <- function(libname, pkgname) {
  shiny::removeInputHandler('nodeOut')
  shiny::removeInputHandler('linksTable')
}
