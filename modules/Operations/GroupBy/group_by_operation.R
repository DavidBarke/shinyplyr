group_by_operation_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::div(
    class = "group-by-op-container grid-gap",
    shiny::uiOutput(
      outputId = ns("columns")
    )
  )
}

group_by_operation <- function(
  input, output, session, .values, data_r
) {
  
  ns <- session$ns
  
  choices_r <- shiny::reactive({
    names(data_r())
  })
  
  output$columns <- shiny::renderUI({
    shiny::selectInput(
      inputId = ns("group_by_columns"),
      label = NULL,
      choices = choices_r(),
      selected = choices_r()[1],
      multiple = TRUE
    )
  })
  
  group_by_data_r <- shiny::reactive({
    # Don't wrap input in shiny::req, because NULL will group by no variables
    # and therefore ungroup a dataset
    data_r() %>%
      dplyr::group_by_at(input$group_by_columns)
  })
  
  return_list <- list(
    data_r = group_by_data_r
  )
  
  return(return_list)
}