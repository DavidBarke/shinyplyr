m_row_ui <- function(id, row_html_id, index) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    htmltools::div(
      id = row_html_id,
      class = "row-container",
      htmltools::div(
        class = "area-step grid-center",
        index
      ),
      shiny::uiOutput(
        outputId = ns("sr_toggle"),
        class = "area-sr-toggle grid-center"
      ),
      htmltools::div(
        class = "area-predicate",
        shiny::selectInput(
          inputId = ns("predicate"),
          label = NULL,
          choices = c(
            "select",
            "filter",
            "mutate",
            "group_by",
            "summarise",
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
        class = "area-result grid-center"
      ),
      htmltools::div(
        class = "area-remove grid-center",
        m_action_button(
          inputId = ns("remove"),
          label = NULL,
          icon = shiny::icon("times")
        )
      ),
      shiny::uiOutput(
        outputId = ns("subrows"),
        class = "subrows-container"
      )
    )
  )
}

m_row <- function(
  input, output, session, .values, data_r, name_r, id_r, row_index, remove_row_fun
) {
  
  ns <- session$ns
  
  # Force evaluation of row_index, otherwise tab_name_r takes always the maximum
  # row index
  force(row_index)
  
  output$operation <- shiny::renderUI({
    switch(
      shiny::req(input$predicate),
      "select" = select_operation_ui(
        id = ns("id_select_operation")
      ),
      "filter" = filter_operation_ui(
        id = ns("id_filter_operation")
      ),
      "mutate" = mutate_operation_ui(
        id = ns("id_mutate_operation")
      ),
      "group_by" = group_by_operation_ui(
        id = ns("id_group_by_operation")
      ),
      "summarise" = summarise_operation_ui(
        id = ns("id_summarise_operation")
      ),
      "plot" = plot_operation_ui(
        id = ns("id_plot_operation")
      )
    )
  })
  
  output$sr_toggle <- shiny::renderUI({
    if (shiny::req(input$predicate) == "plot") {
      htmltools::div(
        class = "sr-toggle-btn",
        m_action_button(
          inputId = ns("sr_toggle"),
          label = NULL,
          icon = toggled_icon_r()
        )
      )
    }
  })
  
  toggle_rv <- shiny::reactiveVal(0)
  
  toggled_icon_r <- shiny::reactive({
    if (toggle_rv() %% 2 == 0) {
      shiny::icon("caret-down")
    } else {
      shiny::icon("caret-right")
    }
  })
  
  shiny::observeEvent(input$sr_toggle, {
    toggle_rv(toggle_rv() + 1)
  })
  
  output$subrows <- shiny::renderUI({
    if (shiny::req(input$predicate) == "plot") {
      plot_subrows_ui(
        id = ns("id_plot_operation")
      )
    } else {
      NULL
    }
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
      "mutate" = mutate_operation_return$data_r(),
      "group_by" = group_by_operation_return$data_r(),
      "summarise" = summarise_operation_return$data_r(),
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
  
  mutate_operation_return <- shiny::callModule(
    module = mutate_operation,
    id = "id_mutate_operation",
    .values = .values,
    data_r = data_r
  )
  
  group_by_operation_return <- shiny::callModule(
    module = group_by_operation,
    id = "id_group_by_operation",
    .values = .values,
    data_r = data_r
  )
  
  summarise_operation_return <- shiny::callModule(
    module = summarise_operation,
    id = "id_summarise_operation",
    .values = .values,
    data_r = data_r
  )
  
  plot_operation_return <- shiny::callModule(
    module = plot_operation,
    id = "id_plot_operation",
    .values = .values,
    data_r = data_r,
    row_index = row_index,
    sr_toggle_r = toggle_rv
  )
  
  return_list <- list(
    data_r = operated_data_r,
    predicate_r = shiny::reactive(shiny::req(input$predicate))
  )
  
  return(return_list)
}

