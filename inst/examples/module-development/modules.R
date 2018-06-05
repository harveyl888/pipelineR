
library(shiny)
library(pipelineR)

library(igraph)


jntModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(8,
             jointPipelineOutput(ns('jnt'), height='600px')
      ),
      column(4,
             fluidRow(
               column(10, offset = 1,
                      actionButton(ns('butRun'), label = '', icon = icon('play'), class = 'btn btn-success', onclick="Shiny.onInputChange('pauseProcess', false)"),
                      actionButton(ns('butPause'), label = '', icon = icon('pause'), class = 'btn btn-warning', onclick="Shiny.onInputChange('pauseProcess', true)"),
                      actionButton(ns('butReset'), label = '', icon = icon('rotate-left'), class = 'btn btn-danger', onclick="Shiny.onInputChange('pauseProcess', false)")
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
      verbatimTextOutput(ns('txtDetails'))
    )
  )

}

## Server code
jntModule <- function(input, output, session, l.nodeTypes) {

  value <- reactiveValues(myNodes = list(),
                          lastNodeId = NULL)

  print(2)

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

  print(3)


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



  # Perform a depth-first search on a pipeline.  Initial node is determined in jointPipeline.js
  # by identifying the first node without an input.  If all nodes have input then the first
  # node added is used.
  pipelineDFS1 <- function()
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
  isDAG1 <- function() {
    df.links <- input$jnt_links
    if (nrow(df.links) > 0) {
      g <- make_directed_graph(edges = unlist(as.vector(t(df.links[, c('source_id', 'target_id')]))))
      return(is_dag(g))
    } else {
      return(TRUE)
    }
  }

  # Determine if pipeline contains input ports with no data
  openInputs1 <- function() {
    df.ports <- input$jnt_ports
    if (nrow(df.ports) > 0) {
      numOpenPorts = nrow(df.ports[df.ports$port_type == 'in' & df.ports$connected == FALSE, ])
      return(numOpenPorts > 0)
    } else {
      return(TRUE)
    }
  }


  # Change the status of a node.  Status is shown by changing the border around the node.
  changeStatus1 <- function(id = NULL, status = NULL) {
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
  deleteButton1 <- function(id = NULL, state = TRUE) {
    if (is.null(id)) return()
    session$sendCustomMessage(type = 'deleteButton',
                              message = list(id = id, state = state))
  }



  ## Run the workflow from the first node or a specified one
  runNodes <- function(continueFrom = NULL) {

    ns <- session$ns

    ## Set button states
    session$sendCustomMessage("disableButton", list(button = ns('butRun'), disabled = TRUE))
    session$sendCustomMessage("disableButton", list(button = ns('butReset'), disabled = TRUE))

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
    if (!isDAG1()) {
      error <- TRUE
      showModal(modalDialog(title = 'Graph Error',
                            'Error in graph.  Not directed acyclic',
                            easyClose = TRUE))
    }

    ## Do we have an open input?
    if (openInputs1()) {
      error <- TRUE
      showModal(modalDialog(title = 'Graph Error',
                            'Error in graph.  Check for open inputs',
                            easyClose = TRUE))
    }

    if(!error) {
      runNodeOrder <- pipelineDFS1()
      allDisplayedNodes <- runNodeOrder

      ## hide delete buttons
      sapply(allDisplayedNodes, function(x) deleteButton1(id = x, state = FALSE))

      if (!is.null(continueFrom)) {  # start from a specific node
        startNodeRef <- match(continueFrom, sapply(runNodeOrder, function(x) x$id))
      } else {
        startNodeRef <- 1
      }

      value$output <- ''

      runNodeOrder <- runNodeOrder[startNodeRef:length(runNodeOrder)]

      sapply(runNodeOrder, function(x) changeStatus1(id = x$id, status = 'queued'))
      for (node in runNodeOrder) {  # loop through each executable node
        changeStatus1(id = value$myNodes[[node$id]]$id, status = 'running')
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

            # print(p$name)
            # print(node$input)
            # print(value$myNodes[[node$input[[p$name]]]]$output)

            l.parameters <- c(l.parameters, setNames(value$myNodes[[node$input[[p$name]]]]$output, p$name))
          }
        }
        execute <- executeNode(type, package, l.parameters)  # execute the node
        if (execute$result == 'success') {
          changeStatus1(id = value$myNodes[[node$id]]$id, status = 'completed')
          value$myNodes[[node$id]]$status <- 'completed'
          value$myNodes[[node$id]]$output <- execute$output  # store the output
        } else {
          changeStatus1(id = l.myNodes[[node$id]]$id, status = 'error')
          value$myNodes[[node$id]]$status <- 'error'
          showModal(modalDialog(title = 'Node Error',
                                h4(paste0('Error in node: ', value$myNodes[[node$id]]$id)),
                                h5(execute$output),
                                easyClose = TRUE))
          value$restartFrom <- node$id  ## restart from the node with error
          break
        }
        httpuv::service()  # refresh
        if (isTRUE(input$pauseProcess)) {
          if (match(node$id, sapply(runNodeOrder, function(x) x$id)) == length(runNodeOrder)) {  ## last node
            value$restartFrom <- node$id
          } else {
            value$restartFrom <- runNodeOrder[[match(node$id, sapply(runNodeOrder, function(x) x$id)) + 1]]$id
          }
          value$myNodes[[value$restartFrom]]$status <- 'queued'
          break
        }
       }

      ## restore delete buttons
      sapply(allDisplayedNodes, function(x) deleteButton1(id = x, state = TRUE))

    }

    ## Set button states
    session$sendCustomMessage("disableButton", list(button = ns('butRun'), disabled = FALSE))
    session$sendCustomMessage("disableButton", list(button = ns('butReset'), disabled = FALSE))

    lapply(value$myNodes, function(n) {
      print(n$output)
    })


  }




  observeEvent(input$butRun, {
    runNodeOrder <- pipelineDFS1()

    runNodeRef <- sapply(runNodeOrder, function(x) match(x$id, sapply(value$myNodes, function(y) y$id)))

    ## account for completion of entire pipeline
    if (value$myNodes[[runNodeRef[length(runNodeRef)]]]$status == 'completed') {
      startFrom <- value$myNodes[[runNodeRef[length(runNodeRef)]]]$id
    } else {
      startFrom <- NULL
      for (n in value$myNodes[runNodeRef]) {
        if (n$status != 'completed') {
          startFrom <- n$id  ## restart from last non-complete node
          break
        }
      }
    }
    runNodes(continueFrom = startFrom)
  })

  observeEvent(input$butPause, {
    print(6)
  })

  observeEvent(input$butReset, {
    print(7)
  })

  ## Output some details - useful for troubleshooting
  output$txtDetails <- renderPrint({
    lastDropped <- input$jnt_lastDroppedNode
    selected <- input$jnt_selectedNode
    print(paste0('Last dropped node = ', lastDropped['name']))
    print(paste0('Selected node = ', selected['name']))
  })


}

