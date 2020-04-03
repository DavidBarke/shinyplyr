help_page_ui <- function(id, content, desc) {
  ns <- shiny::NS(id)
  
  htmltools::div(
    class = "help-page",
    htmltools::h3(
      desc,
      shiny::uiOutput(
        outputId = ns("navigation"),
        inline = TRUE
      )
    ),
    content,
    htmltools::hr(),
    shiny::actionLink(
      inputId = ns("index"),
      label = "Index"
    )
  )
}

help_page <- function(
  input, output, session, .values
) {
  
  ns <- session$ns
  
  shiny::observeEvent(input$index, {
    .values$help$open("index")
  })
  
  output$navigation <- shiny::renderUI({
    shiny::req(length(.values$help$history_rv()) >= 2)
    m_action_button(
      inputId = ns("back"),
      label = NULL,
      icon = shiny::icon("arrow-left")
    )
  })
  
  shiny::observeEvent(input$back, {
    hist <- .values$help$history_rv()
    last_topic <- hist[2]
    hist <- hist[-1]
    .values$help$history_rv(hist)
    .values$help$open(last_topic)
  })
}