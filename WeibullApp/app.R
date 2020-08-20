#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# source function files
source("../quantile_estimators.R")
source("../plotting_functions.R")
source("../model.R")

# source app component files
source("weibull_paper_panel.R")


expl_time <- seq(0, 200, by = .25)
example_data_list <- list.files(path = "./data/")

# Define UI for application that draws a histogram
ui <- navbarPage(title = "WeibullApp",
    # --- --- Data Picker --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
    navbarMenu(title = "Data Picker",
        # --- Datasets ---
        tabPanel("Datasets",
            sidebarLayout(
                # Sidebar
                sidebarPanel(width = 3,
                    # Data selection
                    selectInput(inputId = "example_data",
                                label = "Please choose an example dataset",
                                choices = example_data_list),
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
        tabPanel("Upload Data", "Coming Soon")
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
                         plotOutput("paper_plot")
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
             dataTableOutput("lm_results")
    ),
    
    # --- --- Weibull Explorer --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
    tabPanel("Weibull Explorer", 
             sidebarLayout(
                 sidebarPanel(width = 3,
                     sliderInput(inputId = "expl_b",
                                 label = "Choose a value for b",
                                 min = 0.1, max = 3, value = 2.5),
                     sliderInput(inputId = "expl_T",
                                 label = "Choose a value for T",
                                 min = min(expl_time), max = max(expl_time)/2,
                                 value = (max(expl_time) + min(expl_time)/2) / 2)
                 ),
                 mainPanel(width = 9,
                     fluidRow(
                         column(6, plotOutput("expl_F")),
                         column(6, plotOutput("expl_lambda"))
                     ),
                     fluidRow(
                         column(6, plotOutput("expl_f")),
                         column(6)
                     )
                 )
             ))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # --- Data Picker --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
    picked_data <- reactive({
        read_csv(paste0("data/", input$example_data))
    })
    
    output$picked_data <- renderDataTable(options = list(scrollX = TRUE), {
        picked_data()
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
    
    output$weibull_params <- renderDataTable(options = list(scrollX = TRUE), {
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
    expl_data <- reactive({
        tibble(x = expl_time,
               f = dweibull(expl_time, shape = input$expl_b, scale = input$expl_T),
               F = pweibull(expl_time, shape = input$expl_b, scale = input$expl_T),
               lambda = f / (1 - F))
    })
    
    output$expl_f <- renderPlot({
        ggplot(data = expl_data(), aes(x = x, y = f)) +
            geom_line() +
            labs(x = "time", y = expression(f(t)), title = "Density Plot") +
            xlim(min(expl_time), max(expl_time))
    })
    
    output$expl_F <- renderPlot({
        ggplot(data = expl_data(), aes(x = x, y = F)) +
            geom_line() +
            labs(x = "time", y = expression(F(t)), title = "Distribution Plot") +
            xlim(min(expl_time), max(expl_time)) +
            ylim(0, 1)
    })
    
    output$expl_lambda <- renderPlot({
        ggplot(data = expl_data(), aes(x = x, y = lambda)) +
            geom_line() +
            labs(x = "time", y = expression(lambda(t)), title = "Failure Rate") +
            xlim(min(expl_time), max(expl_time))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
