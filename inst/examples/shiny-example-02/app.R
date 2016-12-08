library(shiny)
library(jointR)

server <- function(input, output, session) {

  onFlushed(once = TRUE, session = session, fun = function(){
    createNode(x = 50, y = 50, id = 'initialize', ports = c(0, 1), session = session)
    createNode(x = 50, y = 120, id = 'node 1', ports = c(1, 1), session = session)
    createNode(x = 50, y = 190, id = 'node 2', ports = c(1, 1), session = session)
    createNode(x = 50, y = 260, id = 'node 3', ports = c(1, 1), session = session)
  })

  output$jnt1 <- renderJointPipeline(
    jointPipeline()
  )

  output$txt1 <- renderPrint({input$jnt1_selectedNode})

  output$tab1 <- renderTable({input$jnt1_links})

  output$txt2 <- renderPrint({input$jnt1_dfs})

}

ui <- shinyUI(
  fluidPage(
    verbatimTextOutput('txt1'),
#    jointPipelineOutput('jnt1', width=1000, height=500),
    fluidRow(
      column(8 ,jointPipelineOutput('jnt1', height=500))
    ),
    tableOutput('tab1'),
    verbatimTextOutput('txt2')
  )
)

shinyApp(server = server, ui = ui)

