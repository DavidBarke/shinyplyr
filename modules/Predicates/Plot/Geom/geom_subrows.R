geom_subrows_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::uiOutput(
    outputId = ns("subrows"),
    class = "subrows-container geom-subrows-container"
  )
}

geom_subrows <- function(
  input, output, session, .values, data_r, subrow_index, free_aesthetics_r
) {
  
  ns <- session$ns
  
  geom_aes_ui <- list(
    colour = colour_aes_ui,
    linetype = linetype_aes_ui,
    percentage = percentage_aes_ui,
    positive = positive_aes_ui,
    shape = shape_aes_ui
  )
  
  geom_aes_server <- list(
    colour = colour_aes,
    linetype = linetype_aes,
    percentage = percentage_aes,
    positive = positive_aes,
    shape = shape_aes
  )
  
  aes_return_env <- new.env()
  
  purrr::walk(.values$plot$OPTIONAL_AES_NAMES, function(aes) {
    if (aes == "group") return()
    
    aes_return_env[[aes]] <- shiny::callModule(
      module = geom_aes_server[[aes_class(aes)]],
      id = aes %_% "value",
      .values = .values
    )
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
          class = "geom-subrow-content grid-gap",
          htmltools::div(
            class = "grid-vertical-center",
            aes
          ),
          geom_aes_ui[[aes_class(aes)]](
            id = ns(aes %_% "value"),
            aes = aes
          )
        ),
        htmltools::div(
          class = "subrow-remove"
        )
      )
    })
  })
  
  geom_args_r <- shiny::reactive({
    free_aes <- free_aesthetics_r()
    names(free_aes) <- free_aes
    purrr::map(free_aes, function(aes) {
      aes_return_env[[aes]]$value_r()
    })
  })
  
  return_list <- list(
    geom_args_r = geom_args_r
  )
  
  return(return_list)
}