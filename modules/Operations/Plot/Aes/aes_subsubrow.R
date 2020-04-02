aes_subsubrow_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::uiOutput(
    outputId = ns("column")
  )
}

aes_subsubrow <- function(
  input, output, session, .values, choices_r, selected_r
) {
  
  ns <- session$ns
  
  output$column <- shiny::renderUI({
    shiny::selectInput(
      inputId = ns("column"),
      label = NULL,
      choices = choices_r(),
      selected = fallback(input$column, selected_r())
    )
  })
  
  value_r <- shiny::reactive({
    value <- fallback(input$column, selected_r())
    
    if (value %in% choices_r()[[1]]) return(value) else selected_r()
  })
  
  return_list <- list(
    value_r = value_r
  )
  
  return(return_list)
}