library(shiny)
library(jointR)

server <- function(input, output) {

  output$jnt1 <- renderJointCanvas({
    jointCanvas(gridSize = 1, markAvailable = TRUE, restrictTranslate = TRUE, multiLinks = FALSE)
  })

}

ui <- shinyUI(
  fluidPage(
    h5('text at top'),
    jointCanvasOutput('jnt1', width=1000, height=300),
    h5('text at bottom')
  )
)

shinyApp(server = server, ui = ui)

