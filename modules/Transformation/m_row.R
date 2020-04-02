m_row_ui <- function(id, row_container_id, index) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    htmltools::div(
      id = row_container_id,
      class = "row-container",
      htmltools::div(
        class = "row-content",
        htmltools::div(
          class = "grid-center",
          index
        ),
        shiny::uiOutput(
          outputId = ns("sr_toggle"),
          class = "grid-center"
        ),
        htmltools::div(
          # Class needed for disabling
          class = "predicate",
          shiny::selectInput(
            inputId = ns("operation"),
            label = NULL,
            choices = c(
              "select",
              "rename",
              "type",
              "filter",
              "mutate",
              "group_by",
              "summarise",
              "plot"
            )
          )
        ),
        htmltools::div(
          class = "grid-center",
          help_button(
            inputId = ns("help_operation")
          )
        ),
        shiny::uiOutput(
          outputId = ns("operation"),
          # Class needed for disabling
          class = "operation"
        ),
        shiny::uiOutput(
          outputId = ns("result"),
          class = "grid-center"
        ),
        htmltools::div(
          # Class needed for disabling
          class = "grid-center remove",
          m_action_button(
            inputId = ns("remove"),
            label = NULL,
            icon = shiny::icon("times")
          )
        )
      ),
      shiny::uiOutput(
        outputId = ns("subrows"),
        class = "subrows-container grid-gap",
        style = "display: none"
      )
    )
  )
}

m_row <- function(
  input, output, session, .values, data_r, dataset_object_r, row_index, remove_row_fun,
  row_container_id
) {
  
  # Important: don't use dataset_object_r()$get_dataset(), because it represents
  # the non-transformed dataset. Use data_r() instead.
  
  ns <- session$ns
  
  # Force evaluation of row_index, otherwise tab_name_r takes always the maximum
  # row index
  force(row_index)
  
  name_r <- shiny::reactive({
    dataset_object_r()$get_name()
  })
  
  id_r <- shiny::reactive({
    dataset_object_r()$get_id()
  })
  
  shiny::observeEvent(input$help_operation, {
    .values$help$open(shiny::req(input$operation))
  })
  
  output$operation <- shiny::renderUI({
    switch(
      shiny::req(input$operation),
      "select" = select_operation_ui(
        id = ns("id_select_operation")
      ),
      "rename" = rename_operation_ui(
        id = ns("id_rename_operation")
      ),
      "type" = type_operation_ui(
        id = ns("id_type_operation")
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
    if (shiny::req(input$operation) %in%
        c("rename", "type", "plot")
    ) {
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
  
  subrow_selector <- paste(row_container_id, " > .subrows-container")
  
  toggle_rv <- shiny::reactiveVal(1)
  
  shiny::observeEvent(toggle_rv(), {
    if (toggle_rv() %% 2 == 0) {
      shinyjs::show(
        anim = .values$ANIM,
        selector = subrow_selector
      )
    } else {
      shinyjs::hide(
        anim = .values$ANIM,
        selector = subrow_selector
      )
    }
  })
  
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
    switch(
      shiny::req(input$operation),
      "rename" = rename_subrows_ui(
        id = ns("id_rename_operation")
      ),
      "type" = type_subrows_ui(
        id = ns("id_type_operation")
      ),
      "plot" = plot_subrows_ui(
        id = ns("id_plot_operation")
      )
    )
  })
  
  output$result <- shiny::renderUI({
    if (shiny::req(input$operation) == "plot") {
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
    new <- .values$viewer$append_tab(
      tab = shiny::tabPanel(
        title = tab_name_r(),
        value = ns(id_r() %_% "plot"),
        plot_output_ui(
          id = ns(id_r() %_% "plot_output")
        )
      ),
      select = TRUE
    )
    
    if (new) {
      shiny::callModule(
        module = plot_output,
        id = id_r() %_% "plot_output",
        .values = .values,
        plot_r = plot_operation_return$plot_r,
        dataset_object = dataset_object_r(),
        tab_value = ns(id_r() %_% "plot")
      )
    }
  })
  
  shiny::observeEvent(input$open_data, {
    new <- .values$viewer$append_tab(
      tab = shiny::tabPanel(
        title = tab_name_r(),
        value = ns(id_r() %_% "data"),
        data_output_ui(
          id = ns(id_r() %_% "data_output")
        )
      ),
      select = TRUE
    )
    
    if (new) {
      shiny::callModule(
        module = data_output,
        id = id_r() %_% "data_output",
        .values = .values,
        data_r = operated_data_r,
        dataset_object = dataset_object_r(),
        row_index = row_index
      )
    }
  })
  
  operated_data_r <- shiny::reactive({
    switch (
      shiny::req(input$operation),
      "select" = select_operation_return$data_r(),
      "rename" = rename_operation_return$data_r(),
      "type" = type_operation_return$data_r(),
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
  
  rename_operation_return <- shiny::callModule(
    module = rename_operation,
    id = "id_rename_operation",
    .values = .values,
    data_r = data_r,
    row_index = row_index,
    sr_toggle_r = toggle_rv
  )
  
  type_operation_return <- shiny::callModule(
    module = type_operation,
    id = "id_type_operation",
    .values = .values,
    data_r = data_r,
    row_index = row_index,
    sr_toggle_r = toggle_rv
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
    row_container_id = row_container_id
  )
  
  return_list <- list(
    data_r = operated_data_r,
    predicate_r = shiny::reactive(shiny::req(input$operation))
  )
  
  return(return_list)
}

