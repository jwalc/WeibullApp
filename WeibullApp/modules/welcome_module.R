welcomeUI <- function (id, label = "Welcome") {
  ns <- NS(id)
  tagList(
    fluidRow(
             h1("WeibullApp", align = "center"),
             h6("v0.1", align = "center")
    ),
    hr(),
    fluidRow(
      column(width = 3,
             radioButtons(inputId = ns("navigation"),
                          label = "Navigation",
                          choices = c("Start", "Instruction", "About"))
      ),
      column(width = 9,
        uiOutput(ns("main_text"))
      )
    )
  )
}

welcomeServer <- function (id) {
  moduleServer(
    id,
    function (input, output, session) {
      start_text <- tagList(
        withMathJax(),
        h3("START"),
        p("Welcome to the WeibullApp (v0.1). The App is build around estimation methods for quantiles of
          the Weibull distribution. These can then be used to estimate the parameters b and T of the
          distribution function:"),
        h3("$$F(t) = 1 - e^{- \\left( \\frac{t}{T} \\right)^b}$$"),
        p("Select 'Instruction' to find out how to use this App. Check the Documentation if you want to
          learn more about the estimation methods, implementation and features."),
        br(),
        p("This ShinyApp is still under heavy development but basic functionality is already implemented."),
        p("Disclaimer: I do not recommend using the app for any serious data analysis.")
      )
      
      instruction_text <- tagList(
        h3("INSTRUCTION"),
        p("1. To use this App, you will have to choose a dataset first. Use the examples or import your own data using the
          Import Panels. After converting and saving the data you will be able to select it in the 'Data Picker' -> 'Datasets'
          subpanel."),
        p("2. Choose one or more methods for quantile estimation."),
        p("3. On the 'Weibull Paper' Panel you can view a Weibull-Plot of your data and chosen methods. The axis are
          transformed such that the Weibull distribution function looks linear. You can check the estimation results
          and estimated parameters per method."),
        p("4. For more model details choose the 'Parameter Estimation' Panel. There you can additionally view results from the
          underlying linear regression."),
        p("5. If you want to explore the effects the parameters b and T have on shape and form of the Weibull distribution, density
          and failure rate functions, go to the 'Weibull Explorer' panel.")
      )
      
      about_text <- tagList(
        h3("ABOUT"),
        strong("Author:"), a(href = "https://github.com/jwalc", "Jan Walczak"), br(),
        strong("Repository:"), a(href = "https://github.com/jwalc/WeibullApp", "WeibullApp"), br(),
        strong("Documentation PDF:"), a(href = "https://github.com/jwalc/WeibullApp/blob/master/documentation/Documentation.pdf", "Documentation"),
        br(),
        p("The WeibullApp is not meant to be used in any serious form. It is a project I do for fun to improve
          my programming skills with R and Shiny. Any feedback is greatly appreciated.")
      )
      
      output$main_text <- renderUI({
        if (input$navigation == "Start") return(start_text)
        if (input$navigation == "Instruction") return(instruction_text)
        if (input$navigation == "About") return(about_text)
      })
      
    }
  )
}
