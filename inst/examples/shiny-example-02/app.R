library(shiny)
library(jointR)

server <- function(input, output, session) {

  onFlushed(once = TRUE, session = session, fun = function(){
    createNode(x = 50, y = 50, name = 'initialize', ports = c(0, 1), session = session)
    createNode(x = 50, y = 120, name = 'node 1', ports = c(1, 1), session = session)
    createNode(x = 50, y = 190, name = 'node 2', ports = c(1, 1), session = session)
    createNode(x = 50, y = 260, name = 'node 3', ports = c(1, 1), session = session)
  })

  output$jnt1 <- renderJointPipeline(
    jointPipeline()
  )

  ## pipeline execution order
  v <- reactiveValues(out = "")
  observeEvent(input$but1, {
    v$out <- pipelineDFS(jnt = 'jnt1', session = session)
#    highlight(jnt = 'jnt1', id = v$out[1], session = session)
    changeStatus(id = v$out[1], status = 'running', session = session)
    changeStatus(id = v$out[2], status = 'queued', session = session)
  })
  output$txt1 <- renderPrint({ v$out })

  ## table of links
  output$tab1 <- renderTable({input$jnt1_links})

}

ui <- shinyUI(
  fluidPage(
    actionButton('but1', 'Update Order of Execution'),
    verbatimTextOutput('txt1'),
    fluidRow(
      column(8 ,jointPipelineOutput('jnt1', height=500))
    ),
    tableOutput('tab1')
  )
)

shinyApp(server = server, ui = ui)

