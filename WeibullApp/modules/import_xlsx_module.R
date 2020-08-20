xlsxImportUI <- function (id, label = "XLSX Import") {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(width = 3,
        fileInput(inputId = ns("file"),
                  label = label,
                  multiple = FALSE,
                  accept = ".csv"),
        tags$hr(),
        uiOutput(ns("select_sheet")),
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
                           sheet = input$sheet)
      })
      
      output$show_sheet <- renderDataTable({
        req(import_data())
        import_data()
      })
      
      return(import_data)
    }
  )
}
