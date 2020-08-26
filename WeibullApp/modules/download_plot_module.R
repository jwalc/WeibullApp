#' @title Shiny Module for downloading ggplot2 plots
#' 
#' A simple module to let the user download a ggplot2 plot to any directory of their choice.
#' UI consists of a textInput for the filename and a download button
#' 
#' @param ggdata ggplot object

downloadPlotUI <- function(id, label = "Save Plot") {
  ns <- NS(id)
  tagList(
    textInput(inputId = ns("downName"),
              label = "Input Filename",
              placeholder = "filename.png"),
    downloadButton(outputId = ns("downPlot"),
                   label = "Save the Plot")
  )
}

downloadPlotServer <- function(id, ggdata) {
  moduleServer(
    id,
    function (input, output, session) {
      output$downPlot <- downloadHandler(
        filename = function() {
          if (input$downName != "") return(input$downName)
          else return(paste0(Sys.Date(), ".png"))
          },
        content = function(file) {
          ggsave(plot = ggdata(), filename = file)
        }
      )
    }
  )
}
