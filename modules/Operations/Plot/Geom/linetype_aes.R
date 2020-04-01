linetype_aes_ui <- function(id, aes) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    shiny::selectInput(
      inputId = ns("linetype"),
      label = NULL,
      choices = list(
        "Select linetype" = list(
          "blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"
        )
      ),
      selected = "solid"
    )
  )
}

linetype_aes <- function(
  input, output, session, .values, aes
) {
  
  ns <- session$ns
  
  return_list <- list(
    value_r = shiny::reactive(fallback(input$linetype, "solid"))
  )
  
  return(return_list)
}