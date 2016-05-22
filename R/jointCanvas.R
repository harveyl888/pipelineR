#' <Add Title>
#'
#' <Add Description>
#'
#' @import htmlwidgets
#'
#' @export
jointCanvas <- function(gridSize = 10,
                        markAvailable = TRUE,
                        restrictTranslate = FALSE,
                        multiLinks = FALSE,
                        width = NULL,
                        height = NULL) {

  # forward options using x
  x = list(
    gridSize = gridSize,
    markAvailable = markAvailable,
    restrictTranslate = restrictTranslate,
    multiLinks = multiLinks
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'jointCanvas',
    x = x,
    width = width,
    height = height,
    package = 'jointR'
  )
}

#' Shiny bindings for jointCanvas
#'
#' Output and render functions for using jointCanvas within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a jointCanvas
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name jointCanvas-shiny
#'
#' @export
jointCanvasOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'jointCanvas', width, height, package = 'jointR')
}

#' @rdname jointCanvas-shiny
#' @export
renderJointCanvas <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, jointCanvasOutput, env, quoted = TRUE)
}
