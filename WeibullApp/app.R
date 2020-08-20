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
library(readxl)

# source function files
source("functions/quantile_estimators.R")
source("functions/plotting_functions.R")
source("functions/model.R")
source("functions/data_converter.R")

# source Shiny modules
source("modules/import_csv_module.R")
source("modules/weibull_explorer_module.R")
source("modules/import_xlsx_module.R")
source("modules/converter_module.R")

# set global variables
example_data_list <- list.files(path = "./data/")

# Define UI
ui <- navbarPage(theme = shinytheme("slate"),
                 title = "WeibullApp",
    tabPanel(title = "Welcome", "Coming Soon"),
    
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
                             csvImportUI("import_csv")
                         ),
                         mainPanel(width = 9,
                             dataTableOutput("import_table")
                         )
                    )
                 ),
                 wellPanel(
                     dataConverterUI("convert_csv")
                 )
        ),
        tabPanel("Import Excel",
                 wellPanel(
                    xlsxImportUI("import_xlsx")
                 ),
                 wellPanel(
                     dataConverterUI("convert_xlsx")
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
    imported_data <- csvImportServer("import_csv")
    
    imported_data_2 <- xlsxImportServer("import_xlsx")
    
    # Data Converter
    output$import_table <- renderDataTable(options = list(scrollX = TRUE), {
        req(imported_data())
        imported_data()
    })
    
    dataConverterServer("convert_csv", imported_data)
    dataConverterServer("convert_xlsx", imported_data_2)
    
    
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
