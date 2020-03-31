help_predicate_ui <- function(id) {
  ns <- shiny::NS(id)
  
  help_page_ui(
    id = ns("id_help_page"),
    content = htmltools::tagList(
      htmltools::p(
        "The predicate determines the operation, that is applied to the dataset
      in a transformation step. All predicates except plot are names of
      dplyr functions. You can find details about the specific operations here:
      "
      ),
      shiny::uiOutput(
        outputId = ns("operation_links"),
        container = htmltools::tags$ul
      )
    )
  )
}

help_predicate <- function(
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
  
  shiny::callModule(
    module = help_page,
    id = "id_help_page",
    .values = .values
  )
}