select_operation_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::div(
    class = "select-op-container",
    shiny::uiOutput(
      outputId = ns("select")
    )
  )
}

select_operation <- function(
  input, output, session, .values, data_r
) {
  
  ns <- session$ns
  
  choices_r <- shiny::reactive({
    names(data_r())
  })
  
  output$select <- shiny::renderUI({
    shiny::selectInput(
      inputId = ns("selected_columns"),
      label = NULL,
      choices = choices_r(),
      selected = choices_r(),
      multiple = TRUE
    )
  })
  
  selected_data_r <- shiny::reactive({
    data_r() %>%
      select(shiny::req(input$selected_columns))
  })
  
  return_list <- list(
    data_r = selected_data_r
  )
  
  return(return_list)
}