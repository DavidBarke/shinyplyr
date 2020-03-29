integer_aes_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::div(
    class = "aes-content integer-aes-content"
  )
}

integer_aes <- function(
  input, output, session, .values
) {
  
  ns <- session$ns
}