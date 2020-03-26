m_row_ui <- function(id, container_id, index) {
  ns <- shiny::NS(id)
  
  htmltools::div(
    id = container_id,
    class = "row-container",
    htmltools::div(
      class = "area-step",
      index
    ),
    htmltools::div(
      class = "area-predicate",
      shiny::selectInput(
        inputId = ns("predicate"),
        label = NULL,
        choices = c(
          "select",
          "filter",
          "plot"
        )
      )
    ),
    shiny::uiOutput(
      outputId = ns("operation"),
      class = "area-operation"
    ),
    shiny::uiOutput(
      outputId = ns("result"),
      class = "area-result"
    ),
    htmltools::div(
      class = "area-remove",
      m_action_button(
        inputId = ns("remove"),
        label = NULL,
        icon = shiny::icon("times")
      )
    )
  )
}

m_row <- function(
  input, output, session, .values, data_r, name_r, id_r, row_index, remove_row_fun
) {
  
  ns <- session$ns
  
  output$operation <- shiny::renderUI({
    switch(
      shiny::req(input$predicate),
      "select" = select_operation_ui(
        id = ns("id_select_operation")
      ),
      "filter" = filter_operation_ui(
        id = ns("id_filter_operation")
      ),
      "plot" = plot_operation_ui(
        id = ns("id_plot_operation")
      )
    )
  })
  
  output$result <- shiny::renderUI({
    if (shiny::req(input$predicate) == "plot") {
      m_action_button(
        inputId = ns("open_plot"),
        label = NULL,
        icon = shiny::icon("chart-bar")
      )
    } else {
      m_action_button(
        inputId = ns("open_data"),
        label = NULL,
        icon = shiny::icon("table")
      )
    }
  })
  
  tab_name_r <- shiny::reactive({
    paste(name_r(), row_index, sep = ": ")
  })
  
  shiny::observeEvent(input$open_plot, {
    plot_output <- plot_operation_return$plot_output_r()
    
    plot_pkg <- plot_operation_return$plot_pkg_r()
    
    new <- .values$home$viewer$append_tab(
      tab = shiny::tabPanel(
        title = tab_name_r(),
        value = ns(id_r() %_% plot_pkg),
        plot_output(
          outputId = ns(id_r() %_% plot_pkg)
        )
      )
    )
    
    if (new) {
      plot_render <- plot_operation_return$plot_render_r()
      
      plot <- plot_operation_return$plot_r()
      
      output[[id_r() %_% plot_pkg]] <- plot_render(plot)
    }
  })
  
  shiny::observeEvent(input$open_data, {
    new <- .values$home$viewer$append_tab(
      tab = shiny::tabPanel(
        title = tab_name_r(),
        value = ns(id_r() %_% "data"),
        DT::dataTableOutput(
          outputId = ns(id_r() %_% "data_output")
        )
      )
    )
    
    if (new) {
      output[[id_r() %_% "data_output"]] <- DT::renderDataTable({
        DT::datatable(operated_data_r())
      })
    }
  })
  
  operated_data_r <- shiny::reactive({
    switch (
      shiny::req(input$predicate),
      "select" = select_operation_return$data_r(),
      "filter" = filter_operation_return$data_r(),
      "plot" = plot_operation_return$data_r()
    )
  })
  
  shiny::observeEvent(input$remove, {
    remove_row_fun()
  })
  
  select_operation_return <- shiny::callModule(
    module = select_operation,
    id = "id_select_operation",
    .values = .values,
    data_r = data_r
  )
  
  filter_operation_return <- shiny::callModule(
    module = filter_operation,
    id = "id_filter_operation",
    .values = .values,
    data_r = data_r
  )
  
  plot_operation_return <- shiny::callModule(
    module = plot_operation,
    id = "id_plot_operation",
    .values = .values,
    data_r = data_r
  )
  
  return_list <- list(
    data_r = operated_data_r
  )
  
  return(return_list)
}

