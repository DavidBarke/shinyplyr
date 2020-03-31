mutate_operation_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::div(
    class = "mutate-op-container"
  )
}

mutate_operation <- function(
  input, output, session, .values, data_r
) {
  
  ns <- session$ns
  
  return_list <- list(
    
  )
  
  return(return_list)
}