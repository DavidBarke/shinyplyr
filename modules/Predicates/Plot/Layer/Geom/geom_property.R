geom_property_ui <- function(id, class) {
  ns <- shiny::NS(id)
  
  shiny::uiOutput(
    outputId = ns("property"), 
    class = paste("property-content", class)
  )
}

geom_property <- function(
  input, output, session, .values, property
) {
  
  ns <- session$ns
  
  
}