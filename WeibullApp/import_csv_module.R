csvImportUI <- function (id, label = "CSV import") {
  ns <- NS(id)
  tagList(
    fileInput(inputId = ns("file"),
              label = label,
              multiple = FALSE,
              accept = ".csv"),
    tags$hr(),
    checkboxInput(inputId = ns("header"),
                  label = "First line are column names",
                  value = TRUE),
    radioButtons(inputId = ns("delimiter"),
                 label = "Which delimiter is used?",
                 choices = c(",", ";"),
                 selected = ","),
    radioButtons(inputId = ns("decimal"),
                 label = "Which symbol is used as the decimal point?",
                 choices = c(".", ","),
                 selected = "."),
    tags$hr(),
    p("Please select a file for importing your data.")
  )
}

csvImportServer <- function (id) {
  moduleServer(
    id,
    function (input, output, session) {
      import_file <- reactive({
        req(input$file)
        input$file
      })
      
      import_data <- reactive({
        readr::read_delim(file = import_file()$datapath,
                          delim = input$delimiter,
                          col_names = input$header,
                          locale = locale(decimal_mark = input$decimal))
      })
      
      return(import_data)
    }
  )
}
