tab_rds_import_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::fluidRow(
    shinydashboard::box(
      width = 12,
      title = "rds-Import",
      rds_import_ui(
        id = ns("id_rds_import")
      ),
      status = "primary"
    )
  )
}

tab_rds_import <- function(
  input, output, session, .values
) {
  
  ns <- session$ns
  
  shiny::callModule(
    module = rds_import,
    id = "id_rds_import",
    .values = .values
  )
}