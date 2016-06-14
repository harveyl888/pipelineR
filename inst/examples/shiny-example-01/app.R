library(shiny)
library(jointR)

server <- function(input, output, session) {

  output$jnt1 <- renderJointCanvas({
    jointCanvas(gridSize = 1, markAvailable = TRUE, restrictTranslate = TRUE, multiLinks = FALSE, border = TRUE)
  })

  output$txtOut <- renderPrint({
    input$jnt1_pipeline
    do.call(rbind, input$jnt1_pipeline$pipeline)
  })

  observeEvent(input$but1, {
    addElement(x=100, y=120, id='node', session)
  })

}

ui <- shinyUI(
  fluidPage(
    h5('text at top'),
    jointCanvasOutput('jnt1', width=1000, height=300),
    h5('text at bottom'),
    actionButton('but1', 'xxx'),
    verbatimTextOutput('txtOut')
  )
)

shinyApp(server = server, ui = ui)

