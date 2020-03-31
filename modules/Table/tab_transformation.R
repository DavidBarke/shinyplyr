tab_transformation_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::fluidRow(
    shinydashboard::box(
      width = 12,
      title = htmltools::tagList(
        "Transformation Table",
        help_button(ns("help_transformation"))
      ),
      m_table_ui(
        id = ns("id_m_table")
      ),
      status = "primary"
    )
  )
}

tab_transformation <- function(
  input, output, session, .values
) {
  
  ns <- session$ns
  
  shiny::observeEvent(input$help_transformation, {
    .values$help$open("transformation")
  })
  
  shiny::callModule(
    module = m_table,
    id = "id_m_table",
    .values = .values
  )
}