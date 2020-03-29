geom_subrows_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::uiOutput(
    outputId = ns("subrows"),
    class = "subrows geom-subrows"
  )
}

geom_subrows <- function(
  input, output, session, .values, data_r, subrow_index, free_aesthetics_r
) {
  
  ns <- session$ns
  
  # purrr::walk(properties, function(property) {
  #   shiny::callModule(
  #     module = geom_property,
  #     id = property %_% "value",
  #     .values = .values
  #   )
  # })
  
  output$subrows <- shiny::renderUI({
    purrr::map2(free_aesthetics_r(), seq_along(free_aesthetics_r()), function(property, index) {
      htmltools::div(
        class = "subrow-container grid-gap m-index",
        htmltools::div(
          class = "subrow-index grid-center",
          paste(subrow_index, index, sep = ".")
        ),
        htmltools::div(
          class = "subrow-content geom-subrow-content grid-gap",
          htmltools::div(
            class = "grid-vertical-center",
            property
          ),
          geom_property_ui(
            id = ns(property %_% "value"),
            class = paste(property, "property-content", sep = "-")
          )
        ),
        htmltools::div(
          class = "subrow-remove"
        )
      )
    })
  })
  
  property_values_r <- shiny::reactive({
    purrr::map_chr(free_aesthetics_r(), function(property) {
      shiny::req(input[[property %_% "value"]])
    })
  })
}