help_operation_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::div(
    class = "help-page",
    htmltools::p(
      "The operation column of the transformation table is dependent on the
      selected predicate. For details see:"
    ),
    shiny::uiOutput(
      outputId = ns("predicate_links")
    )
  )
}

help_operation <- function(
  input, output, session, .values
) {
  
  ns <- session$ns
  
  predicates <- c(
    "data", "select", "rename", "filter", "mutate", "group_by", "summarise", 
    "plot"
  )
  
  output$predicate_links <- shiny::renderUI({
    purrr::map(predicates, function(predicate) {
      shiny::actionLink(
        inputId = ns("help" %_% predicate),
        label = paste("Help:", predicate, "operation.")
      )
    })
  })
  
  purrr::walk(predicates, function(predicate) {
    shiny::observeEvent(input[["help" %_% predicate]], {
      print("observe" %_% predicate)
    })
  })
}