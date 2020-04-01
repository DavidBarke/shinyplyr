init_reactive_vals <- function(.values) {
  .values$dataset_id_rv <- shiny::reactiveVal(NULL)
  
  .values$help_rvs <- shiny::reactiveValues()
}