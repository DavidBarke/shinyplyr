ggplot2_subrow_ui <- function(id, index, html_row_id) {
  ns <- shiny::NS(id)
  
  htmltools::div(
    id = id,
    class = "ggplot2-subrow",
    # Index
    htmltools::div(
      class = "grid-center",
      index
    ),
    # Select layer
    htmltools::div(
      shiny::selectInput(
        inputId = ns("select_layer"),
        label = "Layer",
        choices = c(
          None = "none",
          Aesthetic = "aes",
          Geometry = "geom",
          Label = "label",
          Facet = "facet",
          Coordinates = "coord"
        )
      )
    ),
    # Property
    htmltools::div(
      shiny::uiOutput(
        outputId = ns("property")
      )
    ),
    # Value
    htmltools::div(
      shiny::uiOutput(
        outputId = ns("value")
      )
    )
  )
}

ggplot2_subrow <- function(
  input, output, session, .values
) {
  
  ns <- session$ns
  
  output$property <- shiny::renderUI({
    "Propery"
  })
  
  output$value <- shiny::renderUI({
    "Value"
  })
}