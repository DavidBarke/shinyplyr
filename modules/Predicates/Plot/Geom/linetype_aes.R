linetype_aes_ui <- function(id, aes) {
  ns <- shiny::NS(id)
  
  htmltools::div(
    class = "aes-content linetype-aes-content",
    shiny::selectInput(
      inputId = ns("linetype"),
      label = NULL,
      choices = c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),
      selected = "solid"
    )
  )
}

linetype_aes <- function(
  input, output, session, .values, aes
) {
  
  ns <- session$ns
  
  return_list <- list(
    value_r = shiny::reactive(shiny::req(input$linetype))
  )
  
  return(return_list)
}