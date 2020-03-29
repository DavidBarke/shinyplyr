m_subrow_ui <- function(id, class) {
  ns <- shiny::NS(id)
  
  htmltools::div(
    id = id,
    class = paste("subrow-container", class),
    htmltools::div(
      class = "subrow-index grid-center",
      shiny::textOutput(
        outputId = ns("index")
      )
    ),
    htmltools::div(
      class = "subrow-content",
      shiny::uiOutput(
        outputId = ns("content")
      )
    ),
    htmltools::div(
      class = "subrow-remove grid-center",
      shiny::uiOutput(
        outputId = ns("remove")
      )
    )
  )
}

m_subrow <- function(
  input, output, session, .values, content_ui, content_server, index_r, ...
) {
  
  ns <- session$ns
  
  output$index <- shiny::renderText({
    index_r()
  })
  
  output$content <- shiny::renderUI({
    content_ui(
      id = ns("id_content")
    )
  })
  
  removable_r <- shiny::reactive({
    TRUE
  })
  
  output$remove <- shiny::renderUI({
    if (removable_r()) {
      m_action_button(
        inputId = ns("remove_subrow"),
        label = NULL,
        icon = shiny::icon("times")
      )
    } else {
      NULL
    }
  })
  
  shiny::callModule(
    module = content_server,
    id = "id_content",
    .values = .values,
    ...
  )
  
  return_list <- list(
    remove_r = shiny::reactive(input$remove_subrow)
  )
  
  return(return_list)
}