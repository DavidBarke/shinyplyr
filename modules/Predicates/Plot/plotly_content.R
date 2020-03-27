plotly_content_ui <- function(id, index) {
  ns <- shiny::NS(id)
  
  htmltools::div(
    class = "plotly-content"
  )
}

plotly_content <- function(
  input, output, session, .values
) {
  
  ns <- session$ns
}