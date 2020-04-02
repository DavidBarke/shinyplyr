help_mutate_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    htmltools::p(
      "The mutate operation allows you to create or transform variables. Enter 
      the name of the column that shall be created and select an function, which
      is applied to columns of the current dataset. Depending on the function,
      one ore more columns of a specific column type can be selected. The
      following table shows all functions and the column types to which they can
      be applied:"
    ),
    shiny::uiOutput(
      outputId = ns("mutate_functions"),
      container = htmltools::tags$table
    )
  )
}

help_mutate <- function(
  input, output, session, .values
) {
  
  ns <- session$ns
  
  output$mutate_functions <- shiny::renderUI({
    rows <- purrr::pmap(.values$MUTATE_OPERATORS[c("op", "allowed", "desc")], function(op, allowed, desc) {
      htmltools::tags$tr(
        htmltools::tags$td(op),
        htmltools::tags$td(paste(allowed, collapse = ", ")),
        htmltools::tags$td(desc)
      )
    })
    
    htmltools::tagList(
      htmltools::tags$thead(
        htmltools::tags$tr(
          htmltools::tags$th("Function"),
          htmltools::tags$th("Allowed column types"),
          htmltools::tags$th("Description")
        )
      ),
      htmltools::tags$tbody(
        rows
      )
    )
  })
}