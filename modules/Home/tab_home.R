tab_home_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::fluidRow(
    shinydashboard::box(
      width = 12,
      title = "Data Transformation",
      m_table_ui(
        id = ns("id_m_table")
      ),
      status = "primary"
    ),
    shiny::uiOutput(
      outputId = ns("data_out")
    )
  )
}

tab_home <- function(
  input, output, session, .values
) {
  
  ns <- session$ns
  
  .values$home$viewer <- TabBox$new(
    id = "viewer",
    title = "Results",
    width = 12
  )
  
  .values$home$viewer$set_session(session)
  
  output$data_out <- shiny::renderUI({
    .values$home$viewer$tabBox()
  })
  
  shiny::callModule(
    module = m_table,
    id = "id_m_table",
    .values = .values
  )
}