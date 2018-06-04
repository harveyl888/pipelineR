
library(shiny)
library(pipelineR)


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


  observeEvent(input$butRun, {
    print(value$myNodes)
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

