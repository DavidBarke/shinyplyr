data_output_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    shiny::uiOutput(
      outputId = ns("group_vars")
    ),
    DT::dataTableOutput(
      outputId = ns("data")
    )
  )
}

data_output <- function(
  input, output, session, .values, data_r
) {
  
  rvs <- shiny::reactiveValues(
    # Selection of a new dataset, disconnects all outputs of the previous dataset
    is_disconnected = FALSE
  )
  
  output$data <- DT::renderDataTable({
    DT::datatable(data_r())
  })
  
  group_vars_r <- shiny::reactive({
    group_vars(data_r())
  })
  
  output$group_vars <- shiny::renderUI({
    htmltools::tagList(
      htmltools::tags$b("Grouping Variables:"),
      paste(shiny::req(group_vars_r()), collapse = ", ")
    )
  })
}