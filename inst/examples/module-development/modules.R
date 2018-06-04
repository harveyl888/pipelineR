
library(shiny)
library(pipelineR)


jntModuleUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(8,
           jointPipelineOutput(ns('jnt'), height='600px')
    ),
    column(4,
           fluidRow(
             column(10, offset = 1,
                    actionButton(ns('butRun'), label = '', icon = icon('play'), class = 'btn btn-success', onclick="Shiny.onInputChange('pauseProcess', false)"),
                    actionButton(ns('butPause'), label = '', icon = icon('pause'), class = 'btn btn-warning', onclick="Shiny.onInputChange('pauseProcess', true)"),
                    actionButton('butReset', label = '', icon = icon('rotate-left'), class = 'btn btn-danger', onclick="Shiny.onInputChange('pauseProcess', false)")
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
  )
}

## Server code
jntModule <- function(input, output, session, l.nodeTypes, l.myNodes) {

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
    
}

