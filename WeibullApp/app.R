#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- navbarPage(title = "WeibullApp",
    
    navbarMenu(title = "Data Picker",
        tabPanel("Datasets", "TODO"),
        tabPanel("Upload Data", "TODO")
    ),
    tabPanel("Weibull Paper", "TODO"),
    tabPanel("Parameter Estimation", "TODO"),
    tabPanel("Weibull Explorer", "TODO")
)

# Define server logic required to draw a histogram
server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
