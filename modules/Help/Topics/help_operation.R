help_operation_ui <- function(id) {
  ns <- shiny::NS(id)
  
  help_page_ui(
    id = ns("id_help_page"),
    content = htmltools::tagList(
      htmltools::p(
        "The operation column of the transformation table is dependent on the
      selected",
        shiny::actionLink(
          inputId = ns("help_predicate"),
          label = "predicate"
        ),
        "For details see:"
      ),
      shiny::uiOutput(
        outputId = ns("operation_links"),
        container = htmltools::tags$ul
      )
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
  
  shiny::observeEvent(input$help_predicate, {
    .values$help$open("predicate")
  })
  
  shiny::callModule(
    module = help_page,
    id = "id_help_page",
    .values = .values
  )
}