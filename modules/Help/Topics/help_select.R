help_select_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    htmltools::p(
      "The select operation selects multiple columns from a dataset and removes
      all non-selected columns. The selection depends on the choice of the 
      selection helper:"
    ),
    htmltools::tags$table(
      htmltools::tags$tr(
        htmltools::tags$td("="),
        htmltools::tags$td(
          "Selection by column name"
        )
      ),
      htmltools::tags$tr(
        htmltools::tags$td("starts_with"),
        htmltools::tags$td(
          "Selection by common prefix in column names"
        )
      ),
      htmltools::tags$tr(
        htmltools::tags$td("ends_with"),
        htmltools::tags$td(
          "Selection by common suffix in column names"
        )
      ),
      htmltools::tags$tr(
        htmltools::tags$td("contains"),
        htmltools::tags$td(
          "Selection by common stags$tring in column names"
        )
      ),
      htmltools::tags$tr(
        htmltools::tags$td("matches"),
        htmltools::tags$td(
          "Selection by regular expression"
        )
      )
    ),
    htmltools::h4(
      "Note"
    ),
    htmltools::p(
      "The",
      shiny::actionLink(
        inputId = ns("help_rename"),
        label = "rename operation"
      ),
      "also has a selection functionality comparable to the '=' selection helper."
    )
  )
}

help_select <- function(
  input, output, session, .values
) {
  
  ns <- session$ns
  
  shiny::observeEvent(input$help_rename, {
    .values$help$open("rename")
  })
}