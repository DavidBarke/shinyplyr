summarise_operation_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::div(
    class = "summarise-op-container"
  )
}

summarise_operation <- function(
  input, output, session, .values, data_r
) {
  
  ns <- session$ns
}