facet_layer_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::div(
    class = "facet-layer"
  )
}

facet_layer <- function(
  input, output, session, .values, data_r
) {
  
  ns <- session$ns
}