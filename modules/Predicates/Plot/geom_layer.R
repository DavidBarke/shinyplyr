geom_layer <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::div(
    class = "geom-layer"
  )
}

geom_layer <- function(
  input, output, session, .values, data_r
) {
  
  ns <- session$ns
}