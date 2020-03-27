plot_operation_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    htmltools::div(
      class = "plot-op-container grid-gap",
      htmltools::div(
        shiny::uiOutput(
          outputId = ns("plot_pkg")
        )
      )
    )
  )
}

plot_subrows_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    htmltools::div(
      class = "subrows",
      m_subrows_ui(
        id = ns("id_ggplot2_subrows")
      )
    ),
    htmltools::div(
      class = "subrows",
      m_subrows_ui(
        id = ns("id_plotly_subrows")
      )
    ),
    shiny::uiOutput(
      outputId = ns("add_subrow"),
      class = "add-subrow grid-vertical-center"
    )
  )
}

plot_operation <- function(
  input, output, session, .values, data_r, row_index
) {
  
  ns <- session$ns
  
  # Plot UI --------------------------------------------------------------------
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
  
  # Plot Logic -----------------------------------------------------------------
  output$plot_pkg <- shiny::renderUI({
    ui <- shiny::selectInput(
      inputId = ns("plot_pkg"),
      label = NULL,
      choices = c("ggplot2", "plotly"),
      selected = fallback(input$plot_pkg, NULL)
    )
    
    if (n_active_subrows_r() > 0) {
      ui <- disabled(ui)
    }
    
    ui
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
  
  # Subrows --------------------------------------------------------------------
  output$add_subrow <- shiny::renderUI({
    if (shiny::req(input$plot_pkg) == "ggplot2") {
      m_action_button(
        inputId = ns("add_ggplot2_subrow"),
        label = "Add layer",
        icon = shiny::icon("plus")
      )
    } else {
      m_action_button(
        inputId = ns("add_plotly_subrow"),
        label = "Add subrow",
        icon = shiny::icon("plus")
      )
    }
  })
  
  n_active_subrows_r <- shiny::reactive({
    max(
      ggplot2_subrows_return$n_active_subrows_r(),
      plotly_subrows_return$n_active_subrows_r()
    )
  })
  
  ggplot2_subrows_return <- shiny::callModule(
    module = m_subrows,
    id = "id_ggplot2_subrows",
    .values = .values,
    content_ui = ggplot2_content_ui,
    content_server = ggplot2_content,
    row_index = row_index,
    add_r = shiny::reactive(input$add_ggplot2_subrow),
    subrow_class = "ggplot2-subrow"
  )
  
  plotly_subrows_return <- shiny::callModule(
    module = m_subrows,
    id = "id_plotly_subrows",
    .values = .values,
    content_ui = plotly_content_ui,
    content_server = plotly_content,
    row_index = row_index,
    add_r = shiny::reactive(input$add_plotly_subrow)
  )
  
  return_list <- list(
    data_r = data_r,
    plot_output_r = plot_output_r,
    plot_pkg_r = plot_pkg_r,
    plot_render_r = plot_render_r,
    plot_r = plot_r
  )
}