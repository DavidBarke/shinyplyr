colour_aes_ui <- function(id, aes) {
  ns <- shiny::NS(id)
  
  if (aes == "colour") {
    value = "black"
  } else if (aes == "fill") {
    value = "lightblue"
  }
  
  htmltools::div(
    colourpicker::colourInput(
      inputId = ns("colour"),
      label = NULL,
      value = value
    )
  )
}

colour_aes <- function(
  input, output, session, .values, aes
) {
  
  ns <- session$ns
  
  return_list <- list(
    value_r = shiny::reactive(fallback(input$colour, "black"))
  )
  
  return(return_list)
}