library(shiny)
library(jointR)

server <- function(input, output, session) {
  output$jnt1 <- renderJointPipeline(
    jointPipeline()
  )

}

ui <- shinyUI(
  fluidPage(
    jointPipelineOutput('jnt1', width=1000, height=300)
  )
)

shinyApp(server = server, ui = ui)

