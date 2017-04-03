library(shiny)
library(pipelineR)
library(simpleMathNodes)


### Example app - pipeline engine using graphical pipeline
###
### Nodes are read from an external library (simpleMathNodes).  Their parameters are defined using comments.
### jointPipeline creates a user interface with a stencil canvas on the left and graph canvas on the right.
### Nodes can be dragged from the stencil to the graph and joined by dragging an arrow between an output and
### an input port.

## Define the packages containing nodes to use in this app
## If more than one package is used send as a vector
nodePackages <- 'simpleMathNodes'

## Add all the nodes from the packages to the internal list
includePackages(nodePackages)

## Generate a list of parameters for each type of node
l.nodeTypes <- nodeParameters()

## Temporary folder for uploading files
tempUploadFolder <- paste0(sub('/[^/]*$', '', tempdir()), '/pipeline')
if (!dir.exists(tempUploadFolder)) dir.create(tempUploadFolder)


server <- function(input, output, session) {

  ## space to store nodes for execution
  l.myNodes <- list()
  value <- reactiveValues(pausePipeline = FALSE)

  ## On starting app, populate the stencil canvas with a single copy of each node
  onFlushed(once = TRUE, session = session, fun = function() {
    lapply(seq_along(l.nodeTypes), function(n) {
      x <- 50
      y <- (n-1) * 70 + 50
      ports_in <- length(which(sapply(l.nodeTypes[[n]], function(x) x['type']) == 'nodeinput'))
      ports_out <- 1
      createNode(x = x, y = y, name = names(l.nodeTypes)[n], ports = c(ports_in, ports_out), session = session)
    })
  })

  ## Create the htmlwidget
  output$jnt1 <- renderJointPipeline(
    jointPipeline()
  )

  ## Add a node to the executable list (l.myNodes).
  ## This is triggered when a node is added to the graph canvas.
  observeEvent(input$jnt1_lastDroppedNode, {
    n <- input$jnt1_lastDroppedNode
    l.myNodes[[n['id']]] <<- Node(id = n['id'],
                                  type = n['type'],
                                  name = n['name'],
                                  parameters = l.nodeTypes[[n['type']]])
  })

  ## Update node parameters
  ## Nodes are updated when edited and just before running the workflow
  nodeUpdate <- function(nodeID) {
    l.params <- l.myNodes[[nodeID]][['parameters']]  # Get list of parameters
    for (p in 1:length(l.params)) {  # loop through the parameters
      ## Has the parameter changed?
      if (l.params[[p]][['type']] == 'numeric') {  # update value
        l.params[[p]][['value']] <- input[[paste0(nodeID, '_', l.params[[p]]['name'])]]
      } else if (l.params[[p]][['type']] == 'text') {  # update value
        l.params[[p]][['value']] <- input[[paste0(nodeID, '_', l.params[[p]]['name'])]]
      } else if (l.params[[p]][['type']] == 'file') {  # update value
        uploadFile <- input[[paste0(nodeID, '_', l.params[[p]]['name'])]]
        file.copy(uploadFile$datapath, paste(tempUploadFolder, uploadFile$name, sep = '/'))
        l.params[[p]][['value']] <- paste(tempUploadFolder, uploadFile$name, sep = '/')
      }
    }
    l.myNodes[[nodeID]][['parameters']] <<- l.params  # Update the parameters
  }

  ## Dynamically generate a UI for node parameters.  Triggered when a node is selected.
  output$uiNodeParameters <- renderUI({
    req(input$jnt1_selectedNode)

    ## Grab the selected node id
    nodeID <- input$jnt1_selectedNode['id']

    ## Update parameters for the last selected node before switching to the new one
    if (!is.null(value$lastNodeId)) {
      if (value$lastNodeId != nodeID) {
        nodeUpdate(value$lastNodeId)
      }
    }

    ## Remember this node in order to update the parameters before switching to another
    value$lastNodeId <- nodeID

    ## Build the UI based on the node type
    if (!is.null(l.myNodes[[nodeID]])) {
      params <- l.myNodes[[nodeID]][['parameters']]
      l.widgets <- lapply(params, function(x) {
        if (x['type'] == 'numeric') {
          numericInput(inputId = paste0(nodeID, '_', x['name']),
                       label = x['name'],
                       min = x['min'],
                       max = x['max'],
                       value = x['value'],
                       step = x['step'],
                       width = '50%')
        } else if (x['type'] == 'text') {
          textInput(inputId = paste0(nodeID, '_', x['name']),
                    label = x['name'],
                    value = x['value'],
                    width = '50%')
        } else if (x['type'] == 'file') {
          fileInput(inputId = paste0(nodeID, '_', x['name']),
                    label = x['name'])
        }
      })

      ## Add a heading (node name)
      l.heading <- list(h5(l.myNodes[[nodeID]]['name'], style = "text-align:center; font-weight: bold; color: red"), br())

      ## Return the UI
      l.widgets <- c(l.heading, l.widgets)
      div(id = "nodeParameters", style = "margin: 20px 30px 20px 30px;", do.call(tagList, l.widgets))
    }
  })

  ## Dynamically generate a UI for node output.  Triggered when a node is selected.
  output$uiNodeOutput <- renderUI({
    req(input$jnt1_selectedNode)

    widget <- ''

    ## Grab the selected node id
    nodeID <- input$jnt1_selectedNode['id']

    ## Get the node output and determine its type
    nodeOutput <- l.myNodes[[nodeID]]$output

    if (!is.null(nodeOutput)) {
      nodeOutputType <- class(nodeOutput)

      ## Build the output based on the type
      if (nodeOutputType %in% c('numeric', 'integer', 'character')) {
        widget <- tags$p(nodeOutput)
      }
    }
    div(id = "nodeOutput", style = "margin: 20px 30px 20px 30px;", widget)
  })

  ## Run the workflow from the first node or a specified one
  runNodes <- function(continueFrom = NULL) {

    ## Set button states
    session$sendCustomMessage("disableButton", list(button = 'butRun', disabled = TRUE))
    session$sendCustomMessage("disableButton", list(button = 'butResume', disabled = TRUE))
    session$sendCustomMessage("disableButton", list(button = 'butReset', disabled = TRUE))

    ## Update parameters for the last selected node before switching to the new one
    if (!is.null(value$lastNodeId)) {
      nodeUpdate(value$lastNodeId)
    }

    ## Check for errors
    error <- FALSE

    ## Do any nodes contain an error?
    errorNodes <- which(sapply(l.myNodes, function(x) x$status) == 'error')
    if (length(errorNodes) > 0) {
      error <- TRUE
      showModal(modalDialog(title = 'Node Error',
                            paste0('Error in nodes: ', paste0(sapply(l.myNodes[q], function(x) x$id), collapse = ', ')),
                            easyClose = TRUE))
    }

    ## Do we have a directed acyclic graph?
    if (!isDAG(jnt = 'jnt1', session = session)) {
      error <- TRUE
      showModal(modalDialog(title = 'Graph Error',
                            'Error in graph.  Not directed acyclic',
                            easyClose = TRUE))
    }

    if(!error) {
      runNodeOrder <- pipelineDFS(jnt = 'jnt1', session = session)
      allDisplayedNodes <- runNodeOrder

      ## hide delete buttons
      sapply(allDisplayedNodes, function(x) deleteButton(id = x, state = FALSE, session = session))

      if (!is.null(continueFrom)) {  # start from a specific node
        startNodeRef <- match(continueFrom, runNodeOrder)
      } else {
        startNodeRef <- 1
      }

      ## This currently works for a linear run (not in the case of more than one input to a node)
      if (startNodeRef == 1) {
        lastNodeOutput <- NULL  # starting from first node - no input from previous node
      } else {
        lastNodeOutput <- l.myNodes[[runNodeOrder[startNodeRef - 1]]]$output  # grab output from previous node
      }

      value$output <- ''

      runNodeOrder <- runNodeOrder[startNodeRef:length(runNodeOrder)]

      sapply(runNodeOrder, function(x) changeStatus(id = x, status = 'queued', session = session))
      for (node in runNodeOrder) {  # loop through each executable node
        changeStatus(id = l.myNodes[[node]]$id, status = 'running', session = session)
        type <- l.myNodes[[node]]$type
        parameters <- l.myNodes[[node]]$parameters
        l.parameters <- list()
        for (p in parameters) {  # grab the node function input names and values
          if (p$type %in% c('numeric', 'text')) {
            l.parameters <- c(l.parameters, setNames(p$value, p$name))
          } else if (p$type == 'file') {
            l.parameters <- c(l.parameters, setNames(p$value, p$name))
          } else if (p$type == 'nodeinput') {
            l.parameters <- c(l.parameters, setNames(lastNodeOutput, p$name))
          }
        }
        execute <- executeNode(type, l.parameters)  # execute the node
        if (execute$result == 'success') {
          changeStatus(id = l.myNodes[[node]]$id, status = 'completed', session = session)
          l.myNodes[[node]]$output <<- execute$output  # store the output
          lastNodeOutput <- execute$output  # store output for input into next node
        } else {
          changeStatus(id = l.myNodes[[node]]$id, status = 'error', session = session)
        }
        httpuv::service()  # refresh
        if (isTRUE(session$input$pauseProcess)) {
          if (match(node, runNodeOrder) == length(runNodeOrder)) {  ## last node
            value$restartFrom <- node
          } else {
            value$restartFrom <- runNodeOrder[match(node, runNodeOrder) + 1]
          }
          break
        }
      }

      ## restore delete buttons
      sapply(allDisplayedNodes, function(x) deleteButton(id = x, state = TRUE, session = session))

    }

    ## Set button states
    session$sendCustomMessage("disableButton", list(button = 'butRun', disabled = FALSE))
    session$sendCustomMessage("disableButton", list(button = 'butResume', disabled = FALSE))
    session$sendCustomMessage("disableButton", list(button = 'butReset', disabled = FALSE))

  }

  ## Run through the pipeline, executing each node in turn
  observeEvent(input$butRun, {
    runNodes(continueFrom = NULL)
  })

  observeEvent(input$butPause, {
    resetPipeline(startFrom = value$restartFrom)
  })

  ## Restart the pipeline
  observeEvent(input$butResume, {
    runNodes(continueFrom = value$restartFrom)
  })

  ## Reset pipeline from specific node
  resetPipeline <- function(startFrom = NULL) {
    runNodeOrder <- pipelineDFS(jnt = 'jnt1', session = session)
    if (!is.null(startFrom)) {  # start from a specific node
      startNodeRef <- match(startFrom, runNodeOrder)
    } else {
      startNodeRef <- 1
    }
    runNodeOrder <- runNodeOrder[startNodeRef:length(runNodeOrder)]
    sapply(runNodeOrder, function(x) changeStatus(id = x, status = 'none', session = session))
    for (node in runNodeOrder) {  # loop through each executable node
      l.myNodes[[node]]$output <<- NULL
    }
  }

  ## Reset the pipeline
  observeEvent(input$butReset, {
    value$restartFrom <- NULL
    resetPipeline(startFrom = NULL)
  })

  ## Output some details - useful for troubleshooting
  output$txtDetails <- renderPrint({
    lastDropped <- input$jnt1_lastDroppedNode
    selected <- input$jnt1_selectedNode
    print(paste0('Last dropped node = ', lastDropped['name']))
    print(paste0('Selected node = ', selected['name']))
  })

  ## Output a table of links - useful for troubleshooting
  output$tabLinks <- renderTable({
    input$jnt1_links
  },  striped = TRUE,
  caption = 'Pipeline Links',
  caption.placement = getOption("xtable.caption.placement", "top"),
  caption.width = getOption("xtable.caption.width", NULL)
  )

  ## Update a reactive variable with the executable nodes (for display purposes)
  observeEvent(input$butShowNodes, {
    value$outputNodes <- l.myNodes
  })

  ## Output the executable nodes - useful for troubleshooting
  output$txtShowNodes <- renderPrint({
    value$outputNodes
  })

}

ui <- shinyUI(
  fluidPage(
    tags$head(tags$script(src="buttons.js")),
    br(),
    fluidRow(
      column(8,
             jointPipelineOutput('jnt1', height='600px')
      ),
      column(4,
             fluidRow(
               column(10, offset = 1,
                      actionButton('butRun', label = '', icon = icon('play'), class = 'btn btn-success', onclick="Shiny.onInputChange('pauseProcess', false)"),
                      actionButton('butPause', label = '', icon = icon('pause'), class = 'btn btn-warning', onclick="Shiny.onInputChange('pauseProcess', true)"),
                      actionButton('butResume', label = '', icon = icon('step-forward'), class = 'btn btn-warning', onclick="Shiny.onInputChange('pauseProcess', false)"),
                      actionButton('butReset', label = '', icon = icon('rotate-left'), class = 'btn btn-danger', onclick="Shiny.onInputChange('pauseProcess', false)")
               )
             ),
             div(id = 'divParamsBox', style = 'height: 255px; margin-top: 10px; padding: 10px 10px 10px 10px; border-style: solid; border-radius: 25px',
                 div(id = 'divParams', style = 'height: 235px; overflow-y: scroll',
                     uiOutput('uiNodeParameters')
                 )
             ),
             div(id = 'divOutputBox', style = 'height: 298px; margin-top: 2px; padding: 10px 10px 10px 10px; border-style: solid; border-radius: 25px',
                 div(id = 'divOutput', style = 'height: 278px; overflow: auto',
                     uiOutput('uiNodeOutput')
                 )
             )
      )
    ),
    verbatimTextOutput('txtDetails'),
    tableOutput('tabLinks'),
    actionButton('butShowNodes', 'Show Nodes'),
    verbatimTextOutput('txtShowNodes')
  )
)

shinyApp(server = server, ui = ui)
