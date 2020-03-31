help_mutate_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    htmltools::p(
      "The mutate operation is currently missing."
    )
  )
}

help_mutate <- function(
  input, output, session, .values
) {
  
  ns <- session$ns
}