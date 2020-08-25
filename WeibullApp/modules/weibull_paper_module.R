weibullPaperUI <- function (id, label = "Weibull Paper") {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(width = 2,
                   h4("Select what you want to see on the plot:"),
                   uiOutput(ns("plot_filter"))
      ),
      mainPanel(width = 10,
                fluidRow(
                  wellPanel(
                    h3("Plot of estimated Quantiles on Weibull-Paper"),
                    plotOutput(ns("paper_plot"))
                  )
                ),
                fluidRow(
                  column(width = 6,
                         wellPanel(
                           h3("Weibull Parameters"),
                           dataTableOutput(ns("weibull_params"))
                         )
                  ),
                  column(width = 6,
                         wellPanel(
                           h3("Estimated Quantiles"),
                           dataTableOutput(ns("estimation_data"))
                         )
                  )
                )
      )
    )
  )
}

weibullPaperServer <- function (id, data, methods) {
  moduleServer(
    id,
    function (input, output, session) {
      # calculate range of xlimits --- --- ---
      qplot_x_axis <- reactive({
        req(data())
        
        limits <- weibull_x_limits(data()$time)
        min_val <- max(c(limits[1], base::log10(data()$time) * 0.8))
        max_val <- min(c(limits[2], base::log10(data()$time) * 1.2))
        list(
          limits = limits,
          values = c(min_val, max_val)
        )
      })
      
      # render UI --- --- ---
      output$plot_filter <- renderUI({
        tagList(
          checkboxGroupInput(inputId = session$ns("plot_points"),
                             label = "Which methods shall be shown on the plot?",
                             choices = methods(),
                             selected = methods()),
          checkboxGroupInput(inputId = session$ns("plot_lines"),
                             label = "Which regression lines shall be shown?",
                             choices = methods()),
          sliderInput(inputId = session$ns("xlims"),
                      label = "Modify x-limits",
                      value = 10^c(qplot_x_axis()$values[1], qplot_x_axis()$values[2]),
                      min = 10^qplot_x_axis()$limits[1], max = 10^qplot_x_axis()$limits[2]),
          sliderInput(inputId = session$ns("ylims"),
                      label = "Modify y-limits",
                      value = c(0.1, 99.99),
                      min = 0.1, max = 99.99)
        )
      })
      
      # estimate quantiles --- --- ---
      estimation_data <- reactive({
        req(data())
        weibull_estimation(in_data = data(),
                           estimation_method = methods())
      })
      
      output$estimation_data <- renderDataTable(options = list(scrollX = TRUE), {
        estimation_data()
      })
      
      # calculate parameters --- --- ---
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
      
      # predict lines --- --- ---
      predicted_paths <- reactive({
        req(estimation_data(), weibull_params())
        predict_paths(weibull_params(),
                      x_min = min(estimation_data()$time),
                      x_max = max(estimation_data()$time))
      })
      
      # render plot --- --- ---
      output$paper_plot <- renderPlot({
        req(estimation_data(), predicted_paths())
        weibull_q_plot(in_data = filter(estimation_data(), method %in% input$plot_points),
                       regr_line = filter(predicted_paths(), method %in% input$plot_lines),
                       xmin = input$xlims[1], xmax = input$xlims[2],
                       ymin = input$ylims[1], ymax = input$ylims[2])
        
      })
    }
  )
}
