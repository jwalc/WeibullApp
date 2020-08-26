#' @title Shiny Module for downloading ggplot2 plots
#' 

downloadPlotUI <- function(id, label = "Save Plot") {
  ns <- NS(id)
  tagList(
    textInput(inputId = ns("downName"),
              label = "Input Filename",
              ),
    downloadButton(outputId = ns("downPlot"),
                   label = "Save the Plot",
                   placeholder = "filename.png")
  )
}

downloadPlotServer <- function(id, ggdata) {
  moduleServer(
    id,
    function (input, output, session) {
      output$downPlot <- downloadHandler(
        filename = function() {
          input$downName
          },
        content = function(file) {
          ggsave(plot = ggdata(), filename = file)
        }
      )
    }
  )
}
