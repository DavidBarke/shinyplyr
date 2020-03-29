numeric_aes_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::div(
    class = "aes-content numeric-aes-content"
  )
}

numeric_aes <- function(
  input, output, session, .values
) {
  
  ns <- session$ns
}