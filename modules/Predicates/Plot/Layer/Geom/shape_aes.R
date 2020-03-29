shape_aes_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::div(
    class = "aes-content shape-aes-content"
  )
}

shape_aes <- function(
  input, output, session, .values
) {
  
  ns <- session$ns
}