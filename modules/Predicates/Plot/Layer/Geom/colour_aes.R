colour_aes_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::div(
    class = "aes-content colour-aes-content"
  )
}

colour_aes <- function(
  input, output, session, .values
) {
  
  ns <- session$ns
}