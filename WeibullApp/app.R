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

expl_time <- seq(0, 200, by = 1)

# Define UI for application that draws a histogram
ui <- navbarPage(title = "WeibullApp",
    
    navbarMenu(title = "Data Picker",
        tabPanel("Datasets", "TODO"),
        tabPanel("Upload Data", "TODO")
    ),
    tabPanel("Weibull Paper", "TODO"),
    tabPanel("Parameter Estimation", "TODO"),
    tabPanel("Weibull Explorer", 
             sidebarLayout(
                 sidebarPanel(width = 3,
                     sliderInput(inputId = "expl_b",
                                 label = "Choose a value for b",
                                 min = 0.1, max = 5, value = 2.5),
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
