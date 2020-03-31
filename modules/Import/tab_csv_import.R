tab_csv_import_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::fluidRow(
    shinydashboard::box(
      width = 12,
      title = "csv-Import",
      csv_import_ui(
        id = ns("id_csv_import")
      ),
      status = "primary"
    )
  )
}

tab_csv_import <- function(
  input, output, session, .values
) {
  
  ns <- session$ns
  
  shiny::callModule(
    module = csv_import,
    id = "id_csv_import",
    .values = .values
  )
}