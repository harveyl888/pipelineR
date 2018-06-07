#' @importFrom jsonlite toJSON
#' @export
jointPipeline <- function(
                          nodes,
                          icons = FALSE,
                          width = NULL,
                          height = NULL) {


  ## check node ports
  l.nodes <- list()
  counter <- 0
  for (p in 1:length(nodes)) {  ## loop over parent
    l.child <- list()
    for (n in 1:length(nodes[[p]])) {  ## loop over children
      node <- nodes[[p]][[n]]
      if (is.null(node[['portnames']])) {
        if (is.null(node[['ports']])) {
          portnames <- c('in' = 'in1', 'out' = 'out1')  ## default = one input and one output port
        } else {  ## no portnames - use ports to automatically assign names
          portnames <- c('in' = list(paste0('in', seq(ports[1]))), 'out' = list(paste0('out', seq(ports[2]))))
        }
      } else {  ## portnames exist
        portnames <- node[['portnames']]
      }
      if (is.null(node[['icon']])) {
        nodeicon <- getDefaultIcon()
      } else {
        nodeicon <- node[['icon']]
      }
      if (is.null(node[['name']])) {  ## no name provided - assign one
        counter <- counter + 1
        l.child[[length(l.child) + 1]] <- list(text = paste0('Node_', counter), data = c(level = 1, icon = nodeicon, ports_in = list(portnames[['in']]), ports_out = list(portnames[['out']])))
      } else {
        l.child[[length(l.child) + 1]] <- list(text = node[['name']], data = c(level = 1, icon = nodeicon, ports_in = list(node[['portnames']][['in']]), ports_out = list(node[['portnames']][['out']])))
      }
    }
    l.nodes[[length(l.nodes) + 1]] <- c(text = names(nodes)[p], data = list(list(level = 0)), children = list(l.child))
  }
  # counter <- 0
  # l.nodes <- lapply(nodes, function(x) {
  #   lapply(x, function(y) {
  #     if (is.null(y[['portnames']])) {
  #       if (is.null(y[['ports']])) {
  #         portnames <- c('in' = 'in1', 'out' = 'out1')  ## default = one input and one output port
  #       } else {  ## no portnames - use ports to automatically assign names
  #         portnames <- c('in' = list(paste0('in', seq(ports[1]))), 'out' = list(paste0('out', seq(ports[2]))))
  #       }
  #     } else {  ## portnames exist
  #       portnames <- y[['portnames']]
  #     }
  #     if (is.null(y[['name']])) {  ## no name provided - assign one
  #       counter <- counter + 1
  #       list(name = paste0('Node_', counter), ports_in = portnames[['in']], ports_out = portnames[['out']])
  #     } else {
  #       list(name = y[['name']], ports_in = y[['portnames']][['in']], ports_out = y[['portnames']][['out']])
  #     }
  #   })
  # })

  # forward options using x
  x = list(
    nodes = toJSON(l.nodes, auto_unbox = TRUE),
    icons = icons
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'jointPipeline',
    x = x,
    width = width,
    height = height,
    package = 'pipelineR'
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
  htmlwidgets::shinyWidgetOutput(outputId, 'jointPipeline', width, height, package = 'pipelineR')
}

#' @rdname jointPipeline-shiny
#' @export
renderJointPipeline <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, jointPipelineOutput, env, quoted = TRUE)
}

#' Add a single node to stencil canvas
#'
#' Add one node to the stencil canvas
#'
#' @param x position of node in canvas (x-coordinate) defined in px
#' @param y position of node in canvas (y-coordinate) defined in px
#' @param name node name (displayed as a label)
#' @param portnames list optional named list of port ids.  portnames[['in']] contains a vector of input
#'   port names and portnames[['out']] contains a vector of output port names.  If included then \code{ports}
#'   is ignored.  If omitted then \code{ports} will be used to specify the input and output ports and
#'   names will be automatically assigned.
#' @param ports vector defining number of inputs (in) and outputs (out) for the node
#' @param session Shiny session
#'
#' @export
createNode <- function(x=0, y=0, name=NULL, ports=c('in'=1,'out'=1), portnames, session=shiny::getDefaultReactiveDomain()) {
  if (missing(portnames)) {
    portnames <- c('in' = list(paste0('in', seq(ports[1]))), 'out' = list(paste0('out', seq(ports[2]))))
  }
  session$sendCustomMessage(type = 'createNode',
                            message = list(x = x, y = y, ports_in = portnames[['in']], ports_out = portnames[['out']], name = name))
}
# createNode <- function(x=0, y=0, name=NULL, ports=c('in'=1,'out'=1), session=shiny::getDefaultReactiveDomain()) {
#   session$sendCustomMessage(type = 'createNode',
#                             message = list(x = x, y = y, ports_in = ports[1], ports_out = ports[2], name = name))
# }

#' Add a series of nodes to stencil canvas
#'
#' Add multiple nodes to the stencil canvas
#'
#' @param x position of first node in canvas (x-coordinate) defined in px
#' @param y position of first node in canvas (y-coordinate) defined in px
#' @param offset node offset in y-direction (in px).  Each node will be placed
#' \code{offset px} below the previous one
#' @param name list of node names (displayed as labels on the nodes)
#' @param ports vector defining number of inputs (in) and outputs (out) for the node
#' @param session Shiny session
#'
#' @export
createNodes <- function(x=0, y=0, yOffset=30, name=list(), session=shiny::getDefaultReactiveDomain()) {
  for(i in 1:length(name)) {
    createNode(x = x, y = y + (i-1) * yOffset, name = name[[i]], ports=c(0,1), session = session)
  }
}

#' Highlight a node
#'
#' Highlight a specific node with a box
#'
#' @param jnt reference to htmlwidget
#' @param id node reference id
#' @param session Shiny session
#'
#' @export
highlight <- function(jnt = NULL, id = NULL, session = shiny::getDefaultReactiveDomain()) {
  if (is.null(jnt) | is.null(id)) return()
  session$sendCustomMessage(type = 'highlight',
                            message = list(jnt = jnt, id = id))
}

#' set default icon
#'
#' Set the default icon
#'
#' @param icon name of FontAwesome icon
#'
#' @export
setDefaultIcon <- function(icon) {
  pkg.env$defaultIcon <- icon
}

#' get default icon
#'
#' Get the default icon
#'
#' @return name of default icon
#'
#' @export
getDefaultIcon <- function() {
  return(pkg.env$defaultIcon)
}
