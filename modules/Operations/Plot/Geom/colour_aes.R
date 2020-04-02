colour_aes_ui <- function(id, aes) {
  ns <- shiny::NS(id)
  
  default_col <- if (aes == "colour") "black" else "lightblue"
  
  htmltools::tagList(
    colourpicker::colourInput(
      inputId = ns("colour"),
      label = NULL,
      value = default_col
    )
  )
}

colour_aes <- function(
  input, output, session, .values, aes
) {
  
  ns <- session$ns
  
  default_col <- if (aes == "colour") "black" else "lightblue"
  
  return_list <- list(
    value_r = shiny::reactive(fallback(input$colour, default_col))
  )
  
  return(return_list)
}