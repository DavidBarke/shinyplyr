help_operation_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    htmltools::p(
      "A transformation step takes the dataset from the previous transformation and applies its operation
      according to the specific operation details. The following operations are supported:"
    ),
    shiny::uiOutput(
      outputId = ns("operation_links"),
      container = htmltools::tags$ul
    )
  )
}

help_operation <- function(
  input, output, session, .values
) {
  
  ns <- session$ns
  
  output$operation_links <- shiny::renderUI({
    purrr::map(.values$transformation$PREDICATES, function(predicate) {
      htmltools::tags$li(
        shiny::actionLink(
          inputId = ns("help" %_% predicate),
          label = paste("Help:", predicate, "operation.")
        )
      )
    })
  })
  
  purrr::walk(.values$transformation$PREDICATES, function(predicate) {
    shiny::observeEvent(input[["help" %_% predicate]], {
      .values$help$open(predicate)
    })
  })
}