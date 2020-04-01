show_legend_aes_ui <- function(id, aes) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    shiny::checkboxInput(
      inputId = ns("show_legend"),
      label = NULL,
      value = TRUE
    )
  )
}

show_legend_aes <- function(
  input, output, session, .values, aes
) {
  
  ns <- session$ns
  
  return_list <- list(
    value_r = shiny::reactive(fallback(input$show_legend, TRUE))
  )
  
  return(return_list)
}