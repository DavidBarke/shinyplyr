help_index_ui <- function(id) {
  ns <- shiny::NS(id)
  
  help_page_ui(
    id = ns("id_help_page"),
    content = htmltools::tagList(
      shiny::uiOutput(
        outputId = ns("links"),
        container = htmltools::tags$table
      )
    )
  )
}

help_index <- function(
  input, output, session, .values
) {
  
  ns <- session$ns
  
  output$links <- shiny::renderUI({
    purrr::pmap(.values$help$DATA, function(...) {
      row <- list(...)
      htmltools::tags$tr(
        htmltools::tags$td(
          shiny::actionLink(
            inputId = ns("link" %_% row$topic),
            label = row$title
          )
        ),
        htmltools::tags$td(
          row$desc
        )
      )
    })
  })
  
  purrr::walk(.values$help$DATA$topic, function(topic) {
    shiny::observeEvent(input[["link" %_% topic]], {
      .values$help$open(topic)
    })
  })
  
  shiny::callModule(
    module = help_page,
    id = "id_help_page",
    .values = .values
  )
}