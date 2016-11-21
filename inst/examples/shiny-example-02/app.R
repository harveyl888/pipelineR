library(shiny)
library(jointR)

server <- function(input, output, session) {

  observeEvent(input$but1, {
    createNode(x = 50, y = 50, id = 'myNode', session = session)
  })

  output$jnt1 <- renderJointPipeline(
    jointPipeline()
  )

  output$txt1 <- renderPrint({input$jnt1_selectedNode})

}

ui <- shinyUI(
  fluidPage(
    actionButton('but1', 'button'),
    verbatimTextOutput('txt1'),
    jointPipelineOutput('jnt1', width=1000, height=500)
  )
)

shinyApp(server = server, ui = ui)

