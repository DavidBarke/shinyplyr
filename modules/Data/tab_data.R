tab_data_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::fluidRow(
    shinydashboard::box(
      width = 12,
      title = "Data",
      status = "primary"
    )
  )
}

tab_data <- function(
  input, output, session, .values
) {
  
  ns <- session$ns
}