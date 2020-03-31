help_index_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    shiny::uiOutput(
      outputId = ns("links"),
      container = htmltools::tags$table
    )
  )
}

help_index <- function(
  input, output, session, .values
) {
  
  ns <- session$ns
  
  output$links <- shiny::renderUI({
    sorted_data <- .values$help$DATA %>% arrange(title)
    
    purrr::pmap(sorted_data, function(...) {
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
}