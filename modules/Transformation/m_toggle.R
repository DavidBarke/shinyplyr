m_toggle_button <- function(id, class = "") {
  ns <- shiny::NS(id)
  
  shiny::uiOutput(
    outputId = ns("toggle_btn"),
    class = paste("m-toggle-btn", class)
  )
}

m_toggle_ui <- function(id, ui, start_closed = TRUE) {
  ns <- shiny::NS(id)
  
  if (start_closed) {
    shinyjs::hidden(
      htmltools::div(
        id = ns("toggle_area"),
        ui
      )
    )
  } else {
    htmltools::div(
      id = ns("toggle_area"),
      ui
    )
  }
}

m_toggle <- function(
  input, output, session, .values, start_closed = TRUE
) {
  
  ns <- session$ns
  
  toggle_rv <- shiny::reactiveVal(as.integer(start_closed))
  
  toggled_icon_r <- shiny::reactive({
    if (toggle_rv() %% 2 == 0) {
      shiny::icon("caret-down")
    } else {
      shiny::icon("caret-right")
    }
  })
  
  output$toggle_btn <- shiny::renderUI({
    m_action_button(
      inputId = ns("toggle"),
      label = NULL,
      icon = toggled_icon_r()
    )
  })
  
  shiny::observeEvent(input$toggle, {
    toggle_rv(toggle_rv() + 1)
  })
  
  shiny::observeEvent(toggle_rv(), {
    if (toggle_rv() %% 2 == 0) {
      shinyjs::show(
        anim = .values$ANIM,
        selector = paste0("#", ns("toggle_area"))
      )
    } else {
      shinyjs::hide(
        anim = .values$ANIM,
        selector = paste0("#", ns("toggle_area"))
      )
    }
  })
  
  return_list <- list(
    open_r = shiny::reactive(toggle_rv() %% 2 == 0)
  )
}