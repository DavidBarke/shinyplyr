m_row_ui <- function(id, container_id) {
  ns <- shiny::NS(id)
  
  htmltools::div(
    id = container_id,
    class = "row-container",
    htmltools::div(
      class = "area-predicate",
      shiny::selectInput(
        inputId = ns("predicate"),
        label = NULL,
        choices = c(
          "select",
          "filter"
        )
      )
    ),
    shiny::uiOutput(
      outputId = ns("operation"),
      class = "area-operation"
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
  input, output, session, .values, data_r, row_index, remove_row_fun
) {
  
  ns <- session$ns
  
  shiny::observeEvent(input$remove, {
    remove_row_fun()
  })
  
  output$operation <- shiny::renderUI({
    switch(
      shiny::req(input$predicate),
      "select" = select_operation_ui(
        id = ns("id_select_operation")
      ),
      "filter" = filter_operation_ui(
        id = ns("id_filter_operation")
      )
    )
  })
  
  operated_data_r <- shiny::reactive({
    switch (
      shiny::req(input$predicate),
      "select" = select_operation_return$data_r(),
      "filter" = filter_operation_return$data_r()
    )
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
  
  return_list <- list(
    data_r = operated_data_r
  )
  
  return(return_list)
}

