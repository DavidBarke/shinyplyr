aes_subsubrow_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::uiOutput(
    outputId = ns("column")
  )
}

aes_subsubrow <- function(
  input, output, session, .values, aes_row, choices_r, discrete_choices_r, 
  continuous_choices_r, positive_choices_r, geom_allowed_r
) {
  
  ns <- session$ns
  
  allowed_column_type_r <- shiny::reactive({
    if (aes_row$name %in% c("x", "y")) {
      geom_allowed_r()[aes_row$name]
    } else {
      aes_row$allowed
    }
  })
  
  required_column_type_r <- shiny::reactive({
    switch(
      allowed_column_type_r(),
      "__ALL__" = NULL,
      "discrete" = "discrete ",
      "continuous" = "continuous ",
      "positive" = "positive"
    )
  })
  
  safe_choices_r <- shiny::reactive({
    choices <- switch(
      allowed_column_type_r(),
      "__ALL__" = choices_r(),
      "discrete" = discrete_choices_r(),
      "continuous" = continuous_choices_r(),
      "positive" = positive_choices_r()
    )
    
    if (aes_row$is_required) choices else c("NULL", choices)
  })
  
  or_null_r <- shiny::reactive({
    switch(
      allowed_column_type_r(),
      "__ALL__" = NULL,
      "or NULL"
    )
  })
  
  selected_r <- shiny::reactive({
    if (aes_row$is_required) {
      safe_choices_r()[aes_row$index]
    } else {
      "NULL"
    }
  })
  
  output$column <- shiny::renderUI({
    choices <- list(as.list(safe_choices_r()))
    
    names(choices) <- paste0("Select a ", required_column_type_r(), "column ", or_null_r())
    
    shiny::selectInput(
      inputId = ns("column"),
      label = NULL,
      choices = choices,
      selected = fallback(input$column, selected_r())
    )
  })
  
  value_r <- shiny::reactive({
    value <- fallback(input$column, selected_r())
    
    if (value %in% safe_choices_r()) return(value) else selected_r()
  })
  
  return_list <- list(
    value_r = value_r
  )
  
  return(return_list)
}