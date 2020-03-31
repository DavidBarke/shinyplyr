help_page_ui <- function(id, content) {
  ns <- shiny::NS(id)
  
  htmltools::div(
    class = "help-page",
    content,
    shiny::actionLink(
      inputId = ns("index"),
      label = "Index"
    )
  )
}

help_page <- function(
  input, output, session, .values
) {
  
  ns <- session$ns
  
  shiny::observeEvent(input$index, {
    .values$help$open("index")
  })
}