plotly_subrow_ui <- function(id, index) {
  ns <- shiny::NS(id)
  
  htmltools::div(
    id = id,
    class = "row-container plotly-subrow",
    # Index
    htmltools::div(
      index
    )
  )
}

plotly_subrow <- function(
  input, output, session, .values
) {
  
  ns <- session$ns
}