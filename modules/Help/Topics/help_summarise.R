help_summarise_ui <- function(id) {
  ns <- shiny::NS(id)
  
  help_page_ui(
    id = ns("id_help_page"),
    content = htmltools::tagList(
      
    )
  )
}

help_summarise <- function(
  input, output, session, .values
) {
  
  ns <- session$ns
  
  shiny::callModule(
    module = help_page,
    id = "id_help_page",
    .values = .values
  )
}