#' @title Shiny Module for importing Excel Data
#' 
#' This module lets the user pick an Excel file (.xls or .xlsx) and import it to the app.
#' The user gets a view of the selected sheet of the file by selecting an available sheet and
#' clicking the submit button.
#' The module returns the selected sheet when the user clicks submit.
#' 
#' @return sheet of the excel file as a tibble

xlsxImportUI <- function (id, label = "XLSX Import") {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(width = 3,
        fileInput(inputId = ns("file"),
                  label = label,
                  multiple = FALSE,
                  accept = c(".xls", ".xlsx")),
        tags$hr(),
        uiOutput(ns("select_sheet")),
        checkboxInput(inputId = ns("col_names"),
                      label = "Use first row as column names?",
                      value = TRUE),
        actionButton(inputId = ns("submit"),
                     label = "Submit")
      ),
      mainPanel(
        dataTableOutput(ns("show_sheet"))
      )
    )
  )
}

xlsxImportServer <- function (id) {
  moduleServer(
    id,
    function (input, output, session) {
      import_file <- reactive({
        req(input$file)
        input$file
      })
      
      output$select_sheet <- renderUI({
        req(import_file())
        selectInput(inputId = session$ns("sheet"),
                    label = "Which sheet do you want to import?",
                    choices = readxl::excel_sheets(import_file()$datapath),
                    multiple = FALSE)
      })
      
      import_data <- eventReactive(input$submit, {
        req(import_file())
        readxl::read_excel(path = import_file()$datapath,
                           sheet = input$sheet,
                           col_names = input$col_names)
      })
      
      output$show_sheet <- renderDataTable(options = list(scrollX = TRUE), {
        req(import_data())
        import_data()
      })
      
      return(import_data)
    }
  )
}
