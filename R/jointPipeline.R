#' @export
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

#' Perform a depth-first search
#'
#' Perform a depth-first search on a pipeline.  Initial node is determined in jointPipeline.js
#' by identifying the first node without an input.  If all nodes have input then the first
#' node added is used.
#'
#' @param jnt reference to htmlwidget
#' @param session Shiny session
#'
#' @import igraph
#'
#' @export
pipelineDFS <- function(jnt = NULL, session=shiny::getDefaultReactiveDomain()) {
  df.links <- session$input[[paste0(jnt, '_links')]]
  if (nrow(df.links) > 0) {
    g <- make_directed_graph(edges = unlist(as.vector(t(df.links[, c('source_id', 'target_id')]))))
    dfs_out <- dfs(g, root = session$input[[paste0(jnt, '_dfsRoot')]], neimode = 'in', order.out = T)
    orderedIDs <- V(g)$name[as.numeric(dfs_out$order.out)]

    l.dfs <- lapply(orderedIDs, function(x) {
      foundInputNode <- df.links[df.links$target_id == x, ]
      if (nrow(foundInputNode) > 0) {
        return(list(id = x, input = as.list(foundInputNode[, c("target_port", "source_id")])))
      } else {
        return(list(id = x, input = list()))
      }
    })
    return(l.dfs)
  } else {
    return(NULL)
  }
}

#' Check if pipeline is a directed acyclic graph
#'
#' Determine if pipeline is a directed acyclic graph.  If graph is cyclic there's a danger of
#' getting caught in an infinite loop.
#'
#' @param jnt reference to htmlwidget
#' @param session Shiny session
#'
#' @import igraph
#'
#' @export
isDAG <- function(jnt = NULL, session=shiny::getDefaultReactiveDomain()) {
  df.links <- session$input[[paste0(jnt, '_links')]]
  if (nrow(df.links) > 0) {
    g <- make_directed_graph(edges = unlist(as.vector(t(df.links[, c('source_id', 'target_id')]))))
    return(is_dag(g))
  } else {
    return(TRUE)
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

#' Change node status
#'
#' Change the status of a node.  Status is shown by changing the border around the node.
#' By default the node status can be one of:
#' \itemize{
#'   \item none - no additional highlighting
#'   \item queued - yellow highlight
#'   \item running - green pulsing highlight
#'   \item completed - green highlight
#'   \item error - red pulsing highlight
#' }
#'
#' @param id node reference id
#' @param status one of none, queued, running, completed or error
#' @param session Shiny session
#'
#' @export
changeStatus <- function(id = NULL, status = NULL, session = shiny::getDefaultReactiveDomain()) {
  if (is.null(id)) return()
  led <- FALSE
  if (status == 'none') {
    led <- 'none'
    pulse <- FALSE
  } else if (status == 'queued') {
    led <- 'yellow'
    pulse <- FALSE
  } else if (status == 'running') {
    led <- 'green'
    pulse <- TRUE
  } else if (status == 'completed') {
    led <- 'green'
    pulse = FALSE
  } else if (status == 'error') {
    led <- 'red'
    pulse <- TRUE
  }
  session$sendCustomMessage(type = 'changeLED',
                            message = list(id = id, color = led, pulse = pulse))
}

#' Change state of delete button
#'
#' Set delete button to visible or hidden
#'
#' @param id node reference id
#' @param state true = visible, false = hidden
#' @param session Shiny session
#'
#' @export
deleteButton <- function(id = NULL, state = TRUE, session = shiny::getDefaultReactiveDomain()) {
  if (is.null(id)) return()
  session$sendCustomMessage(type = 'deleteButton',
                            message = list(id = id, state = state))
}
