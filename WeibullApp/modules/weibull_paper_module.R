#' @title Shiny Module for Weibull Paper Panel in WeibullApp
#' 
#' 

weibullPaperUI <- function (id, label = "Weibull Paper") {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(width = 2,
                   # Filtering methods --- ---
                   h4("Select what you want to see on the plot:"),
                   uiOutput(ns("plot_filter")),
                   
                   # Changing labels --- ---
                   h4("Change labels"),
                   textInput(ns("plot_title"),
                             label = "Title:",
                             placeholder = "Title"),
                   textInput(ns("plot_subtitle"),
                             label = "Subtitle",
                             placeholder = "Subtitle"),
                   textInput(ns("plot_xlabel"),
                             label = ("x-axis"),
                             placeholder = "x-axis"),
                   textInput(ns("plot_ylabel"),
                             label = "y-axis",
                             placeholder = "y-axis"),
                   actionButton(ns("apply_labels"),
                                label = "Apply changes"),
                   
                   # Download plot --- ---
                   hr(),
                   h4("Download the Plot as a .png"),
                   downloadPlotUI(ns("savePlot"))
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

weibullPaperServer <- function (id, data, methods, quantiles_df, params_df) {
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
      
      # render datatables --- --- ---
      output$estimation_data <- renderDataTable(options = list(scrollX = TRUE), {
        quantiles_df()
      })
      
      output$weibull_params <- renderDataTable(options = list(scrollX = TRUE), {
        params_df()
      })
      
      # predict lines --- --- ---
      predicted_paths <- reactive({
        req(quantiles_df(), params_df())
        predict_paths(params_df(),
                      x_min = min(quantiles_df()$time),
                      x_max = max(quantiles_df()$time))
      })
      
      # render plot --- --- ---
      plot_labels <- eventReactive(input$apply_labels, ignoreNULL = FALSE, {
        labels <- ggplot2::labs(caption = "WeibullApp")
        if (input$plot_title != "") {
          labels$title <- input$plot_title
        }
        if (input$plot_subtitle != "") {
          labels$subtitle <- input$plot_subtitle
        }
        if (input$plot_xlabel != "") {
          labels$x <- input$plot_xlabel
        }
        if (input$plot_ylabel != "") {
          labels$y <- input$plot_ylabel
        }
        labels
      })
      paper_plot <- reactive({
        req(quantiles_df(), predicted_paths())
        weibull_q_plot(in_data = filter(quantiles_df(), method %in% input$plot_points),
                       regr_line = filter(predicted_paths(), method %in% input$plot_lines),
                       xmin = input$xlims[1], xmax = input$xlims[2],
                       ymin = input$ylims[1], ymax = input$ylims[2]) +
          plot_labels()
      })
      output$paper_plot <- renderPlot({
        req(paper_plot())
        paper_plot()
      })
      
      # save plot --- --- ---
      downloadPlotServer("savePlot", paper_plot)
    }
  )
}
