direction_aes_ui <- function(id, aes) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    shiny::selectInput(
      inputId = ns("direction"),
      label = NULL,
      choices = list("Select step direction" = list(
        "vertical - horizontal" = "vh",
        "horizontal - vertical" = "hv"
      ))
    )
  )
}

direction_aes <- function(
  input, output, session, .values, aes
) {
  
  ns <- session$ns
  
  return_list <- list(
    value_r = shiny::reactive(fallback(input$direction, "vh"))
  )
  
  return(return_list)
}