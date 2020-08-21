#' @title Shiny Module for converting and saving data in WeibullApp
#' 
#' The module is used in the WeibullApp to let the user convert imported
#' data to the currently supported format.
#' The user is asked to specify which columns are used as time, event, n_events and sample
#' data and if they are available at all.
#' After typing a filename the user can press the save button to save the
#' converted dataframe as a tibble in data/user_data/

dataConverterUI <- function (id, label = "Data Converter") {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(width = 3,
                   uiOutput(ns("convert_controls")),
                   hr(),
                   textInput(inputId = ns("save_filename"),
                             label = "Save as file",
                             placeholder = "filename.csv"),
                   actionButton(inputId = ns("save_data"),
                                label = "Save")
      ),
      mainPanel(width = 9,
                dataTableOutput(ns("converted_data"))
      )
    )
  )
}

dataConverterServer <- function (id, data) {
  moduleServer(
    id,
    function (input, output, session) {
      # Converting the imported data --- ---
      colnames_imported <- reactive({
        req(data())
        names(data())
      })
      
      output$convert_controls <- renderUI({
        tagList(
          selectInput(inputId = session$ns("convert_time"),
                      label = "Select time column",
                      choices = c(NA, colnames_imported()),
                      selected = NA,
                      multiple = FALSE),
          selectInput(inputId = session$ns("convert_event"),
                      label = "Select event column",
                      choices = c(NA, colnames_imported()),
                      selected = NA,
                      multiple = FALSE),
          selectInput(inputId = session$ns("convert_n_events"),
                      label = "Select n_events column",
                      choices = c(NA, colnames_imported()),
                      selected = NA,
                      multiple = FALSE),
          selectInput(inputId = session$ns("convert_sample"),
                      label = "Select sample column",
                      choices = c(NA, colnames_imported()),
                      selected = NA,
                      multiple = FALSE),
          actionButton(inputId = session$ns("submit_convert"),
                       label = "Convert")
        )
      })
      
      converted_data <- eventReactive(input$submit_convert, {
        convert_data(in_data = data(),
                     time = input$convert_time,
                     event = input$convert_event,
                     n_events = input$convert_n_events,
                     sample = input$convert_sample)
      })
      
      output$converted_data <- renderDataTable(options = list(scrollX = TRUE), {
        req(converted_data())
        converted_data()
      })
      
      # Saving converted data --- ---
      observe({
        req(data())
        if (input$save_data == 0) return()
        save_user_data(input$save_filename, converted_data())
      })
    }
  )
}
