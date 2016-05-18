library(shiny)
library(jointR)

server <- function(input, output) {
  output$jnt1 <- renderJointCanvas(jointCanvas(''))
}

ui <- shinyUI(
  fluidPage(
    h5('text at top'),
    jointCanvasOutput('jnt1', width=600, height=300),
    h5('text at bottom')
  )
)

shinyApp(server = server, ui = ui)

