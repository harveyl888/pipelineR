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


#' shiny module UI to create a pipeline
#'
#' shiny module UI to create a pipeline
#'
#' @param id id
#'
#' @import shiny
#'
#' @export
jntModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      tags$script(HTML(
        '$(document).ready(function() {
          Shiny.addCustomMessageHandler("disableButton", function(x) {
            $("#" + x.button).prop("disabled", x.disabled) });
        });'
      )),
      column(8,
             jointPipelineOutput(ns('jnt'), height='600px')
      ),
      column(4,
             fluidRow(
               column(10, offset = 1,
                      actionButton(ns('butRun'), label = '', icon = icon('play'), class = 'btn btn-success')
               )
             ),
             div(id = ns('divParamsBox'), style = 'height: 255px; margin-top: 10px; padding: 10px 10px 10px 10px; border-style: solid; border-radius: 25px',
                 div(id = ns('divParams'), style = 'height: 235px; overflow-y: scroll',
                     uiOutput(ns('uiNodeParameters'))
                 )
             ),
             div(id = ns('divOutputBox'), style = 'height: 298px; margin-top: 2px; padding: 10px 10px 10px 10px; border-style: solid; border-radius: 25px',
                 div(id = ns('divOutput'), style = 'height: 278px; overflow: auto',
                     uiOutput(ns('uiNodeOutput'))
                 )
             )
      )
    ),
    fluidRow(
      verbatimTextOutput(ns('txtDetails')),
      numericInput(ns('num'), label = 'num', value = 50, min = 0, max = 100)
    )
  )
}



#' shiny module server code to create a pipeline
#'
#' shiny module server code to create a pipeline
#'
#' @param l.nodeTypes List of nodes to include (comes from nodeParameters function)
#'
#' @import shiny
#' @import igraph
#'
#' @export
jntModule <- function(input, output, session, l.nodeTypes) {

  value <- reactiveValues(myNodes = list(),
                          lastNodeId = NULL)

  ## Create the htmlwidget
  output$jnt <- renderJointPipeline({
    l.nodes <- lapply(seq(l.nodeTypes), function(p) {
      parent_name <- names(l.nodeTypes)[[p]]
      l.parent <- lapply(seq(l.nodeTypes[[p]]), function(n) {
        node_name <- names(l.nodeTypes[[p]])[[n]]
        node_icon <- l.nodeTypes[[p]][[n]][['icon']]
        input_ids <- which(sapply(l.nodeTypes[[p]][[n]][['parameters']], function(x) x['type'] == 'nodeinput'))
        if (length(input_ids) > 0) {
          ports_in <- unname(lapply(input_ids, function(x) l.nodeTypes[[p]][[n]][['parameters']][[x]][['name']]))
        } else {
          ports_in <- list()
        }
        ports_out <- list('out')
        list(name = node_name, icon = node_icon, portnames = list('in'=ports_in, 'out'=ports_out))
      })
      setNames(l.parent, names(l.nodeTypes[[p]]))
    })
    l.nodes <- setNames(l.nodes, names(l.nodeTypes))
    jointPipeline(nodes = l.nodes, icons = TRUE)
  })

  ## Add a node to the executable list (value$myNodes).
  # This is triggered when a node is added to the graph canvas.
  observeEvent(input$jnt_lastDroppedNode, {
    n <- input$jnt_lastDroppedNode
    value$myNodes[[n['id']]] <- Node(id = n['id'],
                                     type = n['type'],
                                     name = n['name'],
                                     package = n['parent'],
                                     parameters = l.nodeTypes[[n['parent']]][[n['type']]][['parameters']])
  })


  ## Update node parameters
  ## Nodes are updated when edited and just before running the workflow
  nodeUpdate <- function(nodeID) {

    l.params <- value$myNodes[[nodeID]][['parameters']]  # Get list of parameters

    for (p in 1:length(l.params)) {  # loop through the parameters
      ## Has the parameter changed?
      if (l.params[[p]][['type']] == 'numeric') {  # update value
        l.params[[p]][['value']] <- input[[paste0('inp', '_', l.params[[p]]['name'])]]
      } else if (l.params[[p]][['type']] == 'text') {  # update value
        l.params[[p]][['value']] <- input[[paste0('inp', '_', l.params[[p]]['name'])]]
      } else if (l.params[[p]][['type']] == 'file') {  # update value
        uploadFile <- input[[paste0(nodeID, '_', l.params[[p]]['name'])]]
        file.copy(uploadFile$datapath, paste(tempUploadFolder, uploadFile$name, sep = '/'))
        l.params[[p]][['value']] <- paste(tempUploadFolder, uploadFile$name, sep = '/')
      }
    }
    isolate({value$myNodes[[nodeID]][['parameters']] <- l.params})  # Update the parameters
  }


  ## Dynamically generate a UI for node parameters.  Triggered when a node is selected.
  output$uiNodeParameters <- renderUI({

    req(input$jnt_selectedNode)

    ns <- session$ns

    ## Grab the selected node id
    nodeID <- input$jnt_selectedNode['id']

    ## Update parameters for the last selected node before switching to the new one
    if (!is.null(value$lastNodeId)) {
      if (value$lastNodeId != nodeID) {
        nodeUpdate(value$lastNodeId)
      }
    }

    ## Remember this node in order to update the parameters before switching to another
    value$lastNodeId <- nodeID

    ## Build the UI based on the node type
    if (!is.null(value$myNodes[[nodeID]])) {
      params <- value$myNodes[[nodeID]][['parameters']]
      l.widgets <- lapply(params, function(x) {
        if (x['type'] == 'numeric') {
          numericInput(inputId = paste0(ns('inp'), '_', x['name']),
                       label = x['name'],
                       min = x['min'],
                       max = x['max'],
                       value = x['value'],
                       step = x['step'],
                       width = '50%')
        } else if (x['type'] == 'text') {
          textInput(inputId = paste0(ns('inp'), '_', x['name']),
                    label = x['name'],
                    value = x['value'],
                    width = '50%')
        } else if (x['type'] == 'file') {
          fileInput(inputId = paste0(ns('inp'), '_', x['name']),
                    label = x['name'])
        }
      })

      ## Add a heading (node name)
      l.heading <- list(h5(value$myNodes[[nodeID]]['name'], style = "text-align:center; font-weight: bold; color: red"), br())

      ## Return the UI
      l.widgets <- c(l.heading, l.widgets)
      div(id = ns("nodeParameters"), style = "margin: 20px 30px 20px 30px;", do.call(tagList, l.widgets))
    }
  })


  ## Dynamically generate a UI for node output.  Triggered when a node is selected.
  output$uiNodeOutput <- renderUI({
    req(input$jnt_selectedNode)

    ns <- session$ns

    widget <- ''

    ## Grab the selected node id
    nodeID <- input$jnt_selectedNode['id']

    ## Get the node output and determine its type
    nodeOutput <- value$myNodes[[nodeID]]$output

    if (!is.null(nodeOutput)) {
      nodeOutputType <- class(nodeOutput)

      ## Build the output based on the type
      if (nodeOutputType %in% c('numeric', 'integer', 'character')) {
        widget <- tags$p(nodeOutput)
      }
    }
    div(id = ns("nodeOutput"), style = "margin: 20px 30px 20px 30px;", widget)
  })


  # Perform a depth-first search on a pipeline.  Initial node is determined in jointPipeline.js
  # by identifying the first node without an input.  If all nodes have input then the first
  # node added is used.
  pipelineDFS <- function()
  {
    df.links <- input$jnt_links
    if (nrow(df.links) > 0) {
      g <- make_directed_graph(edges = unlist(as.vector(t(df.links[, c("source_id", "target_id")]))))
      dfs_out <- dfs(g, root = input$jnt__dfsRoot, neimode = "in", order.out = T)
      orderedIDs <- V(g)$name[as.numeric(dfs_out$order.out)]
      l.dfs <- lapply(orderedIDs, function(x) {
        foundInputNode <- df.links[df.links$target_id == x, ]
        if (nrow(foundInputNode) > 0) {
          return(list(id = x, input = as.list(setNames(foundInputNode[["source_id"]],
                                                       foundInputNode[["target_port"]]))))
        }
        else {
          return(list(id = x, input = list()))
        }
      })
      return(l.dfs)
    }
    else {
      return(NULL)
    }
  }


  # Determine if pipeline is a directed acyclic graph.  If graph is cyclic there's a danger of
  # getting caught in an infinite loop.
  isDAG <- function() {
    df.links <- input$jnt_links
    if (nrow(df.links) > 0) {
      g <- make_directed_graph(edges = unlist(as.vector(t(df.links[, c('source_id', 'target_id')]))))
      return(is_dag(g))
    } else {
      return(TRUE)
    }
  }

  # Determine if pipeline contains input ports with no data
  openInputs <- function() {
    df.ports <- input$jnt_ports
    if (nrow(df.ports) > 0) {
      numOpenPorts = nrow(df.ports[df.ports$port_type == 'in' & df.ports$connected == FALSE, ])
      return(numOpenPorts > 0)
    } else {
      return(TRUE)
    }
  }


  # Change the status of a node.  Status is shown by changing the border around the node.
  changeStatus <- function(id = NULL, status = NULL) {
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
                              message = list(id = id[[1]], color = led, pulse = pulse))
  }


  # Set delete button to visible or hidden
  deleteButton <- function(id = NULL, state = TRUE) {
    if (is.null(id)) return()
    session$sendCustomMessage(type = 'deleteButton',
                              message = list(id = id, state = state))
  }


  ## Run the workflow from the first node or a specified one
  runNodes <- function(continueFrom = NULL) {

    ns <- session$ns

    ## Set button states
    session$sendCustomMessage("disableButton", list(button = ns('butRun'), disabled = TRUE))

    ## Update parameters for the last selected node before switching to the new one
    if (!is.null(value$lastNodeId)) {
      nodeUpdate(value$lastNodeId)
    }

    ## Check for errors
    error <- FALSE

    ## Do any nodes contain an error?
    errorNodes <- which(sapply(value$myNodes, function(x) x$status) == 'error')
    if (length(errorNodes) > 0) {
      error <- TRUE
      showModal(modalDialog(title = 'Node Error',
                            paste0('Error in nodes: ', paste0(sapply(value$myNodes[q], function(x) x$id), collapse = ', ')),
                            easyClose = TRUE))
    }

    ## Do we have a directed acyclic graph?
    if (!isDAG()) {
      error <- TRUE
      showModal(modalDialog(title = 'Graph Error',
                            'Error in graph.  Not directed acyclic',
                            easyClose = TRUE))
    }

    ## Do we have an open input?
    if (openInputs()) {
      error <- TRUE
      showModal(modalDialog(title = 'Graph Error',
                            'Error in graph.  Check for open inputs',
                            easyClose = TRUE))
    }

    if(!error) {
      runNodeOrder <- pipelineDFS()
      allDisplayedNodes <- runNodeOrder

      ## hide delete buttons
      sapply(allDisplayedNodes, function(x) deleteButton(id = x, state = FALSE))

      if (!is.null(continueFrom)) {  # start from a specific node
        startNodeRef <- match(continueFrom, sapply(runNodeOrder, function(x) x$id))
      } else {
        startNodeRef <- 1
      }

      value$output <- ''

      runNodeOrder <- runNodeOrder[startNodeRef:length(runNodeOrder)]

      sapply(runNodeOrder, function(x) changeStatus(id = x$id, status = 'queued'))
      for (node in runNodeOrder) {  # loop through each executable node
        changeStatus(id = value$myNodes[[node$id]]$id, status = 'running')
        type <- value$myNodes[[node$id]]$type
        package <- value$myNodes[[node$id]]$package
        parameters <- value$myNodes[[node$id]]$parameters
        l.parameters <- list()
        for (p in parameters) {  # grab the node function input names and values
          if (p$type %in% c('numeric', 'text')) {
            l.parameters <- c(l.parameters, setNames(p$value, p$name))
          } else if (p$type == 'file') {
            l.parameters <- c(l.parameters, setNames(p$value, p$name))
          } else if (p$type == 'nodeinput') {  ## output from last node pushed in as input to current node
            l.parameters <- c(l.parameters, setNames(value$myNodes[[node$input[[p$name]]]]$output, p$name))
          }
        }
        execute <- executeNode(type, package, l.parameters)  # execute the node
        if (execute$result == 'success') {
          changeStatus(id = value$myNodes[[node$id]]$id, status = 'completed')
          value$myNodes[[node$id]]$status <- 'completed'
          value$myNodes[[node$id]]$output <- execute$output  # store the output
        } else {
          changeStatus(id = value$myNodes[[node$id]]$id, status = 'error')
          value$myNodes[[node$id]]$status <- 'error'
          showModal(modalDialog(title = 'Node Error',
                                h4(paste0('Error in node: ', value$myNodes[[node$id]]$id)),
                                h5(execute$output),
                                easyClose = TRUE))
          value$restartFrom <- node$id  ## restart from the node with error
          break
        }
      }

      ## restore delete buttons
      sapply(allDisplayedNodes, function(x) deleteButton(id = x, state = TRUE))

    }

    ## Set button states
    session$sendCustomMessage("disableButton", list(button = ns('butRun'), disabled = FALSE))
  }

  observeEvent(input$butRun, {
    runNodes()
  })

  ## Output some details - useful for troubleshooting
  output$txtDetails <- renderPrint({
    lastDropped <- input$jnt_lastDroppedNode
    selected <- input$jnt_selectedNode
    print(paste0('Last dropped node = ', lastDropped['name']))
    print(paste0('Selected node = ', selected['name']))
  })
}

