#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse)

# source function files
source("../quantile_estimators.R")
source("../plotting_functions.R")
source("../model.R")
source("data_converter.R")

# source Shiny modules
source("import_csv_module.R")
source("weibull_explorer_module.R")

# set global variables
expl_time <- seq(0, 200, by = .25)
example_data_list <- list.files(path = "./data/")

# Define UI
ui <- navbarPage(theme = shinytheme("slate"),
                 title = "WeibullApp",
    # --- --- Data Picker --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
    navbarMenu(title = "Data Picker",
        # --- Datasets ---
        tabPanel("Datasets",
            sidebarLayout(
                # Sidebar
                sidebarPanel(width = 3,
                    # Data selection
                    radioButtons(inputId = "source_data",
                                 label = "Which data do you want to use?",
                                 choices = c("Example Data", "User Data"),
                                 selected = "Example Data"),
                    uiOutput("data_picker"),
                    # Method selection
                    selectizeInput(inputId = "methods",
                                   label = "Please choose estimation methods",
                                   choices = c("Median Rank",
                                               "Nelson",
                                               "Kaplan-Meier",
                                               "Johnson",
                                               "Sudden Death"),
                                   multiple = TRUE)
                ),
                # Main Panel
                mainPanel(
                    fluidRow(
                        dataTableOutput("picked_data")
                    )
                )
            )
        ),
        # --- Upload Data ---
        tabPanel("Import Data",
                 wellPanel(
                     sidebarLayout(
                         sidebarPanel(width = 3,
                             csvImportUI("import_file")
                         ),
                         mainPanel(width = 9,
                             dataTableOutput("import_table")
                         )
                    )
                 ),
                 wellPanel(
                     sidebarLayout(
                         sidebarPanel(width = 3,
                             uiOutput("convert_controls"),
                             tags$hr(),
                             textInput(inputId = "save_filename",
                                       label = "Save as file",
                                       placeholder = "filename.csv"),
                             actionButton(inputId = "save_data",
                                          label = "Save")
                         ),
                         mainPanel(width = 9,
                             dataTableOutput("converted_data")
                         )
                     )
                 )
        )
    ),
    
    # --- --- Weibull Paper --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
    tabPanel("Weibull Paper",
             sidebarLayout(
                 sidebarPanel(width = 2,
                              h4("Select what you want to see on the plot:"),
                              uiOutput("plot_filter")
                 ),
                 mainPanel(width = 10,
                     fluidRow(
                         wellPanel(
                             h3("Plot of estimated Quantiles on Weibull-Paper"),
                             plotOutput("paper_plot")
                         )
                     ),
                     fluidRow(
                         column(width = 6,
                                wellPanel(
                                    h3("Weibull Parameters"),
                                    dataTableOutput("weibull_params")
                                )
                         ),
                         column(width = 6,
                                wellPanel(
                                    h3("Estimated Quantiles"),
                                    dataTableOutput("estimation_data")
                                )
                         )
                     )
                 )
             )
    ),
    
    # --- --- Parameter Estimation --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
    tabPanel("Parameter Estimation",
             fluidRow(
                 h3("Linear Regression"),
                 p("Results from linear regression on transformed time and quantile values:"),
                 dataTableOutput("lm_results")
                 ),
             hr(),
             fluidRow(
                 h3("Weibull Parameters"),
                 p("Estimation of Weibull parameters based on linear regression results:"),
                 dataTableOutput("weibull_params_2")
             )
    ),
    
    # --- --- Weibull Explorer --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
    tabPanel("Weibull Explorer", 
             weibullExplorerUI("explorer")
    )
)

# Define server logic
server <- function(input, output) {
    
    # --- Data Picker --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
    
    # Selection of a .csv file --- ---
    available_data <- reactive({
        if (input$source_data == "Example Data") {
            list.files(path = "./data/", pattern = "*.csv")
        } else {
            list.files(path = "./data/user_data/")
        }
    })
    
    output$data_picker <- renderUI({
        tagList(
            selectInput(inputId = "picked_data",
                        label = "Please choose a dataset",
                        choices = available_data())
        )
    })
    
    picked_data <- reactive({
        if (input$source_data == "Example Data") {
            read_csv(paste0("data/", input$picked_data))
        } else {
            read_csv(paste0("data/user_data/", input$picked_data))
        }
    })
    
    output$picked_data <- renderDataTable(options = list(scrollX = TRUE), {
        req(picked_data())
        picked_data()
        })
    
    # Data Import Panel --- --- ---
    imported_data <- csvImportServer("import_file")
    
    # show imported data
    output$import_table <- renderDataTable(options = list(scrollX = TRUE), {
        req(imported_data())
        imported_data()
    })
    
    # Converting the imported data --- ---
    colnames_imported <- reactive({
        names(imported_data())
    })
    
    output$convert_controls <- renderUI({
        tagList(
            selectInput(inputId = "convert_time",
                        label = "Select time column",
                        choices = c(NA, colnames_imported()),
                        selected = NA,
                        multiple = FALSE),
            selectInput(inputId = "convert_event",
                        label = "Select event column",
                        choices = c(NA, colnames_imported()),
                        selected = NA,
                        multiple = FALSE),
            selectInput(inputId = "convert_n_events",
                        label = "Select n_events column",
                        choices = c(NA, colnames_imported()),
                        selected = NA,
                        multiple = FALSE),
            selectInput(inputId = "convert_sample",
                        label = "Select sample column",
                        choices = c(NA, colnames_imported()),
                        selected = NA,
                        multiple = FALSE),
            actionButton(inputId = "submit_convert",
                         label = "Convert")
        )
    })
    
    converted_data <- eventReactive(input$submit_convert, {
        convert_data(in_data = imported_data(),
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
        req(converted_data())
        if (input$save_data == 0) return()
        save_user_data(input$save_filename, converted_data())
    })
    
    # --- Weibull Paper --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
    output$plot_filter <- renderUI({
        methods <- input$methods
        tagList(
            checkboxGroupInput(inputId = "plot_points",
                               label = "Which methods shall be shown on the plot?",
                               choices = methods,
                               selected = methods),
            checkboxGroupInput(inputId = "plot_lines",
                               label = "Which regression lines shall be shown?",
                               choices = methods)
        )
    })
    
    estimation_data <- reactive({
        req(picked_data())
        weibull_estimation(in_data = picked_data(),
                           estimation_method = input$methods)
    })
    
    output$estimation_data <- renderDataTable(options = list(scrollX = TRUE), {
        estimation_data()
    })
    
    output$paper_plot <- renderPlot({
        req(estimation_data(), predicted_paths())
        weibull_q_plot(in_data = filter(estimation_data(), method %in% input$plot_points),
                       regr_line = filter(predicted_paths(), method %in% input$plot_lines),
                       xlim = estimation_data()$time)

    })
    
    linear_model <- reactive({
        req(estimation_data())
        weibull_model(in_data = estimation_data())
    })
    
    output$lm_results <- renderDataTable(options = list(scrollX = TRUE), {
        linear_model()
    })
    
    weibull_params <- reactive({
        req(linear_model())
        weibull_paramters_from_model(slope = linear_model()$slope,
                                     intercept = linear_model()$intercept,
                                     method = linear_model()$method)
    })
    
    output$weibull_params <- output$weibull_params_2 <- renderDataTable(options = list(scrollX = TRUE), {
        weibull_params()
    })
    
    predicted_paths <- reactive({
        req(picked_data(), weibull_params())
        predict_paths(weibull_params(),
                      x_min = min(estimation_data()$time),
                      x_max = max(estimation_data()$time))
    })
    
    # --- Parameter Estimation --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
    
    # --- Weibull Explorer --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
    weibullExplorerServer("explorer")
}

# Run the application 
shinyApp(ui = ui, server = server)
