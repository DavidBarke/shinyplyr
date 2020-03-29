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
  
  rvs <- shiny::reactiveValues(
    called_aesthetics = character()
  )
  
  geom_aes_ui <- list(
    colour = colour_aes_ui,
    integer = integer_aes_ui,
    linetype = linetype_aes_ui,
    numeric = numeric_aes_ui,
    shape = shape_aes_ui
  )
  
  geom_aes_server <- list(
    colour = colour_aes,
    integer = integer_aes,
    linetype = linetype_aes,
    numeric = numeric_aes,
    shape = shape_aes
  )
  
  shiny::observeEvent(free_aesthetics_r(), {
    new_aesthetics <- free_aesthetics_r()[!free_aesthetics_r() %in% rvs$called_aesthetics]
    
    purrr::walk(new_aesthetics, function(aes) {
      shiny::callModule(
        module = geom_aes_server[[aes_class(aes)]],
        id = ns(aes %_% value),
        .values = .values,
        aes = aes
      )
    })
    
    rvs$called_aesthetics <- c(rvs$called_aesthetics, new_aesthetics)
  })
  
  output$subrows <- shiny::renderUI({
    purrr::map2(free_aesthetics_r(), seq_along(free_aesthetics_r()), function(aes, index) {
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
            aes
          ),
          geom_aes_ui[[aes_class(aes)]](
            id = ns(aes %_% "value")
          )
        ),
        htmltools::div(
          class = "subrow-remove"
        )
      )
    })
  })
  
  aes_values_r <- shiny::reactive({
    purrr::map_chr(free_aesthetics_r(), function(aes) {
      shiny::req(input[[aes %_% "value"]])
    })
  })
}