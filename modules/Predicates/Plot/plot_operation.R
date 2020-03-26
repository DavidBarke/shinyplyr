plot_operation_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::div(
    class = "plot-op-container",
    htmltools::div(
      shiny::selectInput(
        inputId = ns("plot_pkg"),
        label = NULL,
        choices = c("ggplot2", "plotly")
      )
    )
  )
}

plot_operation <- function(
  input, output, session, .values, data_r
) {
  
  ns <- session$ns
  
  plot_render_r <- shiny::reactive({
    switch(
      shiny::req(input$plot_pkg),
      "ggplot2" = shiny::renderPlot,
      "plotly" = plotly::renderPlotly
    )
  })
  
  plot_output_r <- shiny::reactive({
    switch(
      shiny::req(input$plot_pkg),
      "ggplot2" = shiny::plotOutput,
      "plotly" = plotly::plotlyOutput
    )
  })
  
  plot_r <- shiny::reactive({
    switch(
      shiny::req(input$plot_pkg),
      "ggplot2" = ggplot2_plot_r(),
      "plotly" = plotly_plot_r()
    )
  })
  
  plot_pkg_r <- shiny::reactive({
    shiny::req(input$plot_pkg)
  })
  
  ggplot2_plot_r <- shiny::reactive({
    ggplot(mtcars, aes(x = mpg, y = cyl)) +
      geom_point()
  })
  
  plotly_plot_r <- shiny::reactive({
    plot_ly(mtcars, x = ~mpg, y = ~cyl, type = "scatter", mode = "markers")
  })
  
  return_list <- list(
    data_r = data_r,
    plot_output_r = plot_output_r,
    plot_pkg_r = plot_pkg_r,
    plot_render_r = plot_render_r,
    plot_r = plot_r
  )
}