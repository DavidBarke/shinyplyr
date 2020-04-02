viewer_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    shiny::uiOutput(
      outputId = ns("viewer"),
      class = "viewer"
    ),
    m_action_button(
      inputId = ns("close_all"),
      label = "Close all tabs"
    )
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
  
  shiny::observeEvent(input$close_all, {
    purrr::walk(.values$viewer$get("open_tab_values"), function(value) {
      .values$viewer$remove_tab(value)
    })
  })
}