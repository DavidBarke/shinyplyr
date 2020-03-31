viewer_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::uiOutput(
    outputId = ns("viewer")
  )
}

viewer <- function(
  input, output, session, .values
) {
  
  ns <- session$ns
  
  .values$viewer <- TabBox$new(
    id = ns("viewer"),
    title = "Viewer",
    width = 12,
    side = "right"
  )
  
  .values$viewer$set_session(session)
  
  output$viewer <- shiny::renderUI({
    .values$viewer$tabBox()
  })
}