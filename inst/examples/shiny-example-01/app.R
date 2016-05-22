library(shiny)
library(jointR)

server <- function(input, output) {

  output$jnt1 <- renderJointCanvas({
    jointCanvas(gridSize = 1, markAvailable = TRUE, restrictTranslate = TRUE, multiLinks = FALSE)
  })

  output$txtOut <- renderPrint({
    do.call(rbind, input$jnt1_pipeline$pipeline)
  })

}

ui <- shinyUI(
  fluidPage(
    h5('text at top'),
    jointCanvasOutput('jnt1', width=1000, height=300),
    h5('text at bottom'),
    verbatimTextOutput('txtOut')
  )
)

shinyApp(server = server, ui = ui)

