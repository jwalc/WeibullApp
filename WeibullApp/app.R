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

expl_time <- seq(0, 200, by = .25)
example_data_list <- list.files(path = "./data/")

# Define UI for application that draws a histogram
ui <- navbarPage(title = "WeibullApp",
    # --- --- Data Picker --- ---
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
                                   choices = c("Median Rank", "Sudden Death", "Nelson", "Kaplan-Meier", "Johnson"),
                                   multiple = TRUE),
                    # Column selection
                    selectizeInput(inputId = "exists_column",
                                   label = "Which columns do exist in the given data?",
                                   choices = c("Time" = "exists_time",
                                               "Event" = "exists_event",
                                               "Number of Events" = "exists_n_events",
                                               "Sample Identificator" = "exists_sample"),
                                   multiple = TRUE),
                ),
                # Main Panel
                mainPanel(
                    fluidRow(
                        wellPanel(
                            uiOutput("pick_columns")
                        )
                    ),
                    dataTableOutput("picked_data")
                )
            )
        ),
        # --- Upload Data ---
        tabPanel("Upload Data", "Coming Soon")
    ),
    
    # --- --- Weibull Paper --- ---
    tabPanel("Weibull Paper", "Coming Soon"),
    
    # --- --- Parameter Estimation --- ---
    tabPanel("Parameter Estimation", "Coming Soon"),
    
    # --- --- Weibull Explorer --- ---
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
    
    # --- Data Picker --- --- --- --- ---
    picked_data <- reactive({
        read_csv(paste0("data/", input$example_data))
    })
    
    output$pick_columns <- renderUI({
        columns <- colnames(picked_data())
        tagList(
            if("exists_time" %in% input$exists_column) {
                selectInput("example_time", "Please choose the column containing time data", columns)
            },
            if ("exists_event" %in% input$exists_column) {
                selectInput("example_event", "Please choose the column containing type of event", columns)
            },
            if (any(c("Kaplan-Meier", "Johnson") %in% input$methods) & "exists_n_events" %in% input$exists_column) {
                selectInput("example_n_events", "Please choose the column containing data on number of events", columns)
            },
            if ("Sudden Death" %in% input$methods & "exists_sample" %in% input$exists_column) {
                selectInput("example_sample", "Please choose the column containing sample identificator", columns)
            }
        )
    })
    
    output$picked_data <- renderDataTable({
        picked_data()
        })
    
    # --- Weibull Paper --- --- --- --- ---
    
    # --- Parameter Estimation --- --- --- --- ---
    
    # --- Weibull Explorer --- --- --- --- ---
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
