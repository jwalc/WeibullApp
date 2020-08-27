#' @title 
#' 

exploreModelUI <- function (id, label = "Explore Model") {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        # Choose Weibull Parameters ---
        uiOutput(ns("paramUI")),
        
        # Choose methods
        uiOutput(ns("filterUI"))
        
        # x-limits ---
        
        # plot labels ---
      ),
      mainPanel(
        # plot output ---
        plotOutput(ns("distr_plot")),
        # show R^2 ---
        
        # show dataTables
        dataTableOutput(ns("regr_results"))
      )
    )
  )
}

exploreModelServer <- function (id, estimated_quantiles, model_data, weibull_params) {
  moduleServer(
    id,
    function (input, output, session) {
      time_limit <- reactive({
        round(
          max(estimated_quantiles()$time) * (1/max(estimated_quantiles()$F_i)) * 1.5
          )
      })
      
      methods <- reactive({
        unique(estimated_quantiles()$method)
      })
      
      # UI for parameter selection ---
      output$paramUI <- renderUI({
        tagList(
          sliderInput(inputId = session$ns("param_b"),
                      label = "b",
                      min = 0.1,
                      max = 5,
                      value = 1),
          sliderInput(inputId = session$ns("param_T"),
                      label = "T",
                      min = 0,
                      max = time_limit(),
                      value = max(estimated_quantiles()$time) / 2)
        )
      })
      
      # filtering for methods ---
      output$filterUI <- renderUI({
        tagList(
          checkboxGroupInput(inputId = session$ns("filter"),
                             label = "Methods",
                             choices = methods())
        )
      })
      
      filtered_data <- reactive({
        estimated_quantiles() %>%
          filter(method %in% input$filter)
      })
      
      # render Plot ---
      distr_line <- reactive({
        tibble(time = seq(0, time_limit(), length.out = 1000),
               F = pweibull(time, shape = input$param_b, scale = input$param_T))
      })
      
      output$distr_plot <- renderPlot({
        ggplot2::ggplot(data = distr_line(), aes(x = time, y = F)) +
          geom_line() +
          geom_point(data = filtered_data(), mapping = aes(x = time, y = F_i, color = method)) +
          ylim(c(0, 1)) +
          labs(title = "Weibull Fit",
               x = "time",
               y = "Quantiles")
      })
      
      # render dataTables ---
      output$regr_results <- renderDataTable({
        model_data() %>%
          dplyr::full_join(., weibull_params(), by = "method") %>%
          dplyr::select(method, b, T, R_squared) %>%
          dplyr::arrange(desc(R_squared))
      })
    }
  )
}
