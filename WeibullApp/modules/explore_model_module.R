#' @title 
#' 

exploreModelUI <- function (id, label = "Explore Model") {
  ns <- NS(id)
  tagList(
    withMathJax(),
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
        wellPanel(
          plotOutput(ns("distr_plot"))
        ),
        # show R^2 ---
        wellPanel(  
          fluidRow(
            column(3,
                   h3("$$R^2$$ for your selected parameters:", align = "center")
            ),
            column(9,
                   tableOutput(ns("r_squared"))
            )
          )
        ),
        # show dataTables
        wellPanel(
          fluidRow(
            column(3,
                   h3("Best fit using linear regression on transformed data:", align = "center")
            ),
            column(9,
                   tableOutput(ns("regr_results"))
            )
          )
        )
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
      
      # calculate R^2
      transformed_data <- reactive({
        estimated_quantiles() %>%
          mutate(y_transform = log(log(1 / (1 - F_i))),
                 x_transform = log(time))
      })
      
      r_squared <- eventReactive(input$param_b | input$param_T, {
        result <- tibble(method = NULL,
                         R_squared = NULL)
        
        for (m in methods()) {
          params <- model_data() %>%
            filter(method == m)
          
          df <- transformed_data() %>%
            filter(method == m) %>%
            mutate(residuals = y_transform - (- input$param_b * base::log(input$param_T) + input$param_b * x_transform))
          
          y_bar <- mean(df$y_transform)
          
          SSR <- sum((df$residuals)^2)
          SST <- sum((df$y_transform - y_bar)^2)

          result <- rbind(result, tibble(
            method = m,
            R_squared = 1 - (SSR/SST)
          ))
        }
        result
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
      
      # render Tables ---
      output$regr_results <- renderTable({
        model_data() %>%
          dplyr::full_join(., weibull_params(), by = "method") %>%
          dplyr::select(method, b, T, R_squared) %>%
          dplyr::arrange(desc(R_squared))
      })
        
      output$r_squared <- renderTable({
        r_squared()
      })
    }
  )
}
