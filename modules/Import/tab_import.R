tab_import_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::fluidRow(
    shinydashboard::box(
      width = 12,
      title = "Import",
      m_import_ui(
        id = ns("id_m_import")
      ),
      status = "primary"
    ),
    shiny::uiOutput(
      outputId = ns("data_out")
    )
  )
}

tab_import <- function(
  input, output, session, .values
) {
  
  ns <- session$ns
  
  .values$import$viewer <- TabBox$new(
    id = "viewer",
    title = "Preview",
    width = 12
  )
  
  .values$import$viewer$set_session(session)
  
  output$data_out <- shiny::renderUI({
    .values$import$viewer$tabBox()
  })
  
  shiny::callModule(
    module = m_import,
    id = "id_m_import",
    .values = .values
  )
}