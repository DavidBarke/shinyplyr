tab_home_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::fluidRow(
    shinydashboard::box(
      width = 12,
      title = "Home",
      status = "primary"
    )
  )
}

tab_home <- function(
  input, output, session, .values
) {
  
  ns <- session$ns
}