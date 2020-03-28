plot_content_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::div(
    class = "plot-content grid-gap",
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

plot_content <- function(
  input, output, session, .values
) {
  
  ns <- session$ns
  
  output$property <- shiny::renderUI({
    "Property"
  })
  
  output$value <- shiny::renderUI({
    "Value"
  })
}