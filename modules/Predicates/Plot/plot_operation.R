plot_operation_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    htmltools::div(
      class = "plot-op-container",
      htmltools::div(
        shiny::selectInput(
          inputId = ns("plot_pkg"),
          label = NULL,
          choices = c("ggplot2", "plotly")
        )
      ),
      htmltools::div(
        class = "plot-op-add-subrow",
        shiny::uiOutput(
          outputId = ns("add_subrow"),
          class = "vertical-center"
        )
      )
    )
  )
}

plot_operation <- function(
  input, output, session, .values, data_r, row_html_id, row_index
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
        label = "Add subrow",
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
  
  ggplot2_rvs <- shiny::reactiveValues(
    n_subrow = 0,
    max_subrow = 0
  )
  
  plotly_rvs <- shiny::reactiveValues(
    n_subrow = 0,
    max_subrow = 0
  )
  
  prev_subrow_selector <- function(n, pkg) {
    if (n == 1) {
      paste0("#", row_html_id)
    } else {
      paste0("#", ns(pkg %_% "subrow" %_% (n - 1)))
    }
  }
  
  hide_prev_label <- function(n, pkg) {
    if (n > 1) {
      shinyjs::hide(
        selector = paste(prev_subrow_selector(n, pkg), "label")
      )
    }
  }
  
  shiny::observeEvent(input$add_ggplot2_subrow, {
    ggplot2_rvs$n_subrow <- ggplot2_rvs$n_subrow + 1
    
    hide_prev_label(ggplot2_rvs$n_subrow, "ggplot2")
    
    ui <- ggplot2_subrow_ui(
      id = ns("ggplot2_subrow" %_% ggplot2_rvs$n_subrow),
      index = paste(row_index, ggplot2_rvs$n_subrow, sep = ".")
    )
    
    shiny::insertUI(
      selector = prev_subrow_selector(ggplot2_rvs$n_subrow, "ggplot2"),
      where = "afterEnd",
      ui = ui
    )
    
    if (ggplot2_rvs$n_subrow > ggplot2_rvs$max_subrow) {
      ggplot2_rvs$max_subrow <- ggplot2_rvs$n_subrow
      
      shiny::callModule(
        module = ggplot2_subrow,
        id = "ggplot2_subrow" %_% ggplot2_rvs$n_subrow,
        .values = .values
      )
    }
  })
  
  shiny::observeEvent(input$add_plotly_subrow, {
    plotly_rvs$n_subrow <- plotly_rvs$n_subrow + 1
    
    hide_prev_label(plotly_rvs$n_subrow, "plotly")
    
    ui <- plotly_subrow_ui(
      id = ns("plotly_subrow" %_% plotly_rvs$n_subrow),
      index = paste(row_index, plotly_rvs$n_subrow, sep = ".")
    )
    
    shiny::insertUI(
      selector = prev_subrow_selector(plotly_rvs$n_subrow, "plotly"),
      where = "afterEnd",
      ui = ui
    )
    
    if (plotly_rvs$n_subrow > plotly_rvs$max_subrow) {
      plotly_rvs$max_subrow <- plotly_rvs$n_subrow
      
      shiny::callModule(
        module = plotly_subrow,
        id = "plotly_subrow" %_% plotly_rvs$n_subrow,
        .values = .values
      )
    }
  })
  
  return_list <- list(
    data_r = data_r,
    plot_output_r = plot_output_r,
    plot_pkg_r = plot_pkg_r,
    plot_render_r = plot_render_r,
    plot_r = plot_r
  )
}