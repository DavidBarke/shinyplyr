dec_to_asc <- Vectorize(function(x) rawToChar(as.raw(x)))
shape_names <- c(
  "square", "circle", "triangle point up", "plus", "cross", "diamond", 
  "triangle point down", "square cross", "star", "diamond plus", "circle plus",
  "triangles up and down", "square plus", "circle cross", "square and triangle down",
  "filled square", "filled circle", "filled triangle point up", "filled diamond",
  "solid circle", "bullet", "filled circle fill", "filled square fill",
  "filled diamond fill", "filled triangle point up fill", "filled triangle point down fill"
)

shape_aes_ui <- function(id, aes) {
  ns <- shiny::NS(id)
  
  choices <- c(0:25, 33:126)
  names(choices) <- paste(choices, c(shape_names, dec_to_asc(33:126)), sep = ": ")
  
  htmltools::div(
    shiny::selectInput(
      inputId = ns("shape"),
      label = NULL,
      choices = choices,
      selected = "16"
    )
  )
}

shape_aes <- function(
  input, output, session, .values, aes
) {
  
  ns <- session$ns
  
  return_list <- list(
    value_r = shiny::reactive(as.integer(shiny::req(input$shape)))
  )
  
  return(return_list)
}