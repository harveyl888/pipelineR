library(shiny)
library(pipelineR)
library(simpleMathNodes)

### Example app - pipeline engine using graphical pipeline
###
### Nodes are read from an external library (simpleMathNodes).  Their parameters are defined using comments.
### jointPipeline creates a user interface with a stencil canvas on the left and graph canvas on the right.
### Nodes can be dragged from the stencil to the graph and joined by dragging an arrow between an output and
### an input port.

## implementation using shiny module

source('modules.R')

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

## Choose a default icon
setDefaultIcon('circle-o')

server <- function(input, output, session) {

  ## call module server code
  callModule(jntModule, 'jnt1', l.nodeTypes)
}

ui <- shinyUI(
  fluidPage(
    tags$head(tags$script(src="buttons.js")),
    br(),
    jntModuleUI('jnt1')
  )
)

shinyApp(server = server, ui = ui)
