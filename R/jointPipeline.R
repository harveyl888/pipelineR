
jointPipeline <- function(
                          width = NULL,
                          height = NULL) {

  # forward options using x
  x = list()

  # create widget
  htmlwidgets::createWidget(
    name = 'jointPipeline',
    x = x,
    width = width,
    height = height,
    package = 'jointR'
  )
}


#' Shiny bindings for jointPipeline
#'
#' Output and render functions for using jointPipeline within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a jointPipeline
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name jointPipeline-shiny
#'
#' @export
jointPipelineOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'jointPipeline', width, height, package = 'jointR')
}

#' @rdname jointPipeline-shiny
#' @export
renderJointPipeline <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, jointPipelineOutput, env, quoted = TRUE)
}

#' @export
createNode <- function(x=0, y=0, id=NULL, session=shiny::getDefaultReactiveDomain()) {
  session$sendCustomMessage(type = 'createNode',
                            message = list(x = x, y = y, name=id))
}

