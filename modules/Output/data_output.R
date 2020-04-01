data_output_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    shiny::uiOutput(
      outputId = ns("connected_state")
    ),
    shiny::uiOutput(
      outputId = ns("group_vars")
    ),
    DT::dataTableOutput(
      outputId = ns("data")
    ),
    data_export_ui(
      id = ns("id_data_export")
    )
  )
}

data_output <- function(
  input, output, session, .values, data_r, dataset_object, row_index
) {
  
  # Important: don't use dataset_object$get_dataset(), because it represents
  # the non-transformed dataset. Use data_r() instead.
  
  ns <- session$ns
  
  rvs <- shiny::reactiveValues(
    static_data = NULL
  )
  
  name_r <- shiny::reactive({
    dataset_object$get_name()
  })
  
  id_r <- shiny::reactive({
    dataset_object$get_id()
  })
  
  is_connected_r <- shiny::reactive({
    .values$dataset_id_rv() == id_r()
  })
  
  shiny::observeEvent(data_r(), {
    if (is_connected_r()) {
      rvs$static_data <- data_r()
    }
  })
  
  output_data_r <- shiny::reactive({
    if (is_connected_r()) {
      data_r()
    } else {
      rvs$static_data
    }
  })
  
  output$data <- DT::renderDataTable({
    DT::datatable(output_data_r())
  })
  
  group_vars_r <- shiny::reactive({
    group_vars(output_data_r())
  })
  
  output$group_vars <- shiny::renderUI({
    htmltools::tagList(
      htmltools::tags$b("Grouping Variables:"),
      paste(shiny::req(group_vars_r()), collapse = ", ")
    )
  })

  output$connected_state <- shiny::renderUI({
    shiny::req(!is_connected_r())
    htmltools::tags$b(glue::glue(
      "Table is temporarily disconnected, because the selected dataset is not
      equal to {name_r()}. If you select {name_r()}, the connection will be
      reestablished." 
    ))
  })
  
  shiny::callModule(
    module = data_export,
    id = "id_data_export",
    .values = .values,
    data_r = data_r,
    name_r = name_r,
    row_index = row_index
  )
}