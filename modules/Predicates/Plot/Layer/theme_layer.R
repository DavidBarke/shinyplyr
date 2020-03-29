theme_layer_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::div(
    class = "theme-layer"
  )
}

theme_layer <- function(
  input, output, session, .values, data_r
) {
  
  ns <- session$ns
}