tab_transformation_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::fluidRow(
    shinydashboard::box(
      width = 12,
      title = htmltools::tagList(
        "Data Transformation",
        help_button(ns("help_transformation"))
      ),
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

tab_transformation <- function(
  input, output, session, .values
) {
  
  ns <- session$ns
  
  .values$transformation$viewer <- TabBox$new(
    id = "viewer",
    title = "Results",
    width = 12,
    side = "right"
  )
  
  .values$transformation$viewer$set_session(session)
  
  output$data_out <- shiny::renderUI({
    .values$transformation$viewer$tabBox()
  })
  
  shiny::observeEvent(input$help_transformation, {
    .values$help$open("transformation")
  })
  
  shiny::callModule(
    module = m_table,
    id = "id_m_table",
    .values = .values
  )
}