positive_aes_ui <- function(id, aes) {
  ns <- shiny::NS(id)
  
  value <- switch(
    aes,
    "bins" = 30,
    "size" = 0.5
  )
  
  step <- switch(
    aes,
    "bins" = 1,
    size = 0.5
  )
  
  htmltools::tagList(
    shiny::numericInput(
      inputId = ns("value"),
      label = NULL,
      value = value,
      min = 0,
      step = step
    )
  )
}

positive_aes <- function(
  input, output, session, .values, aes
) {
  
  ns <- session$ns
  
  default_value <- switch(
    aes,
    "bins" = 30,
    "size" = 0.5
  )
  
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
      value <- safe_numeric_input_value(fallback(input$value, default_value))
      if (is.na(value)) value <- default_value
      print(value)
      max(value, 0)
    })
  )
  
  return(return_list)
}