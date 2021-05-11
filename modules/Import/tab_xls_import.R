tab_xls_import_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::fluidRow(
    shinydashboard::box(
      width = 12,
      title = "xls/xlsx-Import",
      xls_import_ui(
        id = ns("id_xls_import")
      ),
      status = "primary"
    )
  )
}

tab_xls_import <- function(
  input, output, session, .values
) {
  
  ns <- session$ns
  
  shiny::callModule(
    module = xls_import,
    id = "id_xls_import",
    .values = .values
  )
}