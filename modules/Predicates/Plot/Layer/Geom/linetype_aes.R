linetype_aes_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::div(
    class = "aes-content linetype-aes-content"
  )
}

linetype_aes <- function(
  input, output, session, .values
) {
  
  ns <- session$ns
}