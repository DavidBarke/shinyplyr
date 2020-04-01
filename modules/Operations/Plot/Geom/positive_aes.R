positive_aes_ui <- function(id, aes) {
  ns <- shiny::NS(id)
  
  value <- 0.5
  
  htmltools::tagList(
    shiny::numericInput(
      inputId = ns("value"),
      label = NULL,
      value = value,
      min = 0,
      step = 0.5
    )
  )
}

positive_aes <- function(
  input, output, session, .values, aes
) {
  
  ns <- session$ns
  
  debounced_value_r <- shiny::reactive({
    shiny::req(input$value)
  }) %>% debounce(1000)
  
  shiny::observeEvent(debounced_value_r(), {
    if (safe_numeric_input_value(shiny::req(input$value)) < 0) {
      shiny::updateNumericInput(
        session = session,
        inputId = "value",
        value = 0
      )
    }
  })
  
  return_list <- list(
    value_r = shiny::reactive({
      value <- safe_numeric_input_value(fallback(input$value, 0.5))
      max(value, 0)
    })
  )
  
  return(return_list)
}