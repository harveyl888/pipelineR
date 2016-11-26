library(shiny)
library(jointR)

server <- function(input, output, session) {

  observeEvent(input$but1, {
#    createNode(x = 50, y = 50, id = 'myNode', session = session)
    createNodes(x = 50, y = 50, yOffset = 70, id = list('node 1', 'node 2', 'node 3'), session = session)
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
    actionButton('but1', 'button'),
    verbatimTextOutput('txt1'),
    jointPipelineOutput('jnt1', width=1000, height=500),
    tableOutput('tab1'),
    verbatimTextOutput('txt2')
  )
)

shinyApp(server = server, ui = ui)

