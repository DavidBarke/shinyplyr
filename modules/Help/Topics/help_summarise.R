help_summarise_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    htmltools::p(
      "The summarise operation is currently missing."
    )
  )
}

help_summarise <- function(
  input, output, session, .values
) {
  
  ns <- session$ns
}