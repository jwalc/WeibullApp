weibullExplorerUI <- function (id, label = "Weibull Explorer") {
  ns <- NS(id)
  expl_time <- seq(0, 200, by = .25)
  tagList(
    sidebarLayout(
      sidebarPanel(width = 3,
                   sliderInput(inputId = ns("param_b"),
                               label = "Choose a value for b",
                               min = 0.1, max = 3, value = 2.5),
                   sliderInput(inputId = ns("param_T"),
                               label = "Choose a value for T",
                               min = min(expl_time), max = max(expl_time),
                               value = (max(expl_time) + min(expl_time)/2) / 2)
      ),
      mainPanel(width = 9,
                fluidRow(
                  column(6, plotOutput(ns("plot_F"))),
                  column(6, plotOutput(ns("plot_lambda")))
                ),
                fluidRow(
                  column(6, plotOutput(ns("plot_f"))),
                  column(6)
                )
      )
    )
  )
}

weibullExplorerServer <- function (id) {
  moduleServer(
    id,
    function (input, output, session) {
      expl_time <- seq(0, 200, by = .25)
      
      expl_data <- reactive({
        tibble(x = expl_time,
               f = dweibull(expl_time, shape = input$param_b, scale = input$param_T),
               F = pweibull(expl_time, shape = input$param_b, scale = input$param_T),
               lambda = f / (1 - F))
      })
      
      output$plot_f <- renderPlot({
        ggplot(data = expl_data(), aes(x = x, y = f)) +
          geom_line() +
          labs(x = "time", y = expression(f(t)), title = "Density Plot") +
          xlim(min(expl_time), max(expl_time))
      })
      
      output$plot_F <- renderPlot({
        ggplot(data = expl_data(), aes(x = x, y = F)) +
          geom_line() +
          labs(x = "time", y = expression(F(t)), title = "Distribution Plot") +
          xlim(min(expl_time), max(expl_time)) +
          ylim(0, 1)
      })
      
      output$plot_lambda <- renderPlot({
        df <- expl_data()
        # current workaround for plot showing jags
        if (input$param_b > 2) {
          df <- df %>%
            dplyr::filter(lambda < Inf) %>%
            dplyr::arrange(lambda)
          while (sum(diff(df$x) <= 0) > 0) {
            df <- df[which(diff(df$x) > 0), ]
          }
        }
        
        ggplot(data = df, aes(x = x, y = lambda)) +
          geom_line() +
          labs(x = "time", y = expression(lambda(t)), title = "Failure Rate") +
          xlim(min(expl_time), max(expl_time))
      })
    }
  )
}
