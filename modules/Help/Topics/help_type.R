help_type_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    htmltools::p(
      "The type operation allows to change the data type of a column. Only
      columns that are of a supported data type can be changed into another
      supported data type. Currently the following data types are supported:"
    ),
    htmltools::tags$table(
      htmltools::tags$tr(
        htmltools::tags$th(
          htmltools::tags$b("Type")
        ),
        htmltools::tags$th(
          htmltools::tags$b("Value")
        )
      ),
      htmltools::tags$tr(
        htmltools::tags$td("lgl"),
        htmltools::tags$td(
          "TRUE, FALSE, NA"
        )
      ),
      htmltools::tags$tr(
        htmltools::tags$td("int"),
        htmltools::tags$td(
          "Integer numbers, NA"
        )
      ),
      htmltools::tags$tr(
        htmltools::tags$td("dbl"),
        htmltools::tags$td(
          "Real numbers, NA, NaN, Inf"
        )
      ),
      htmltools::tags$tr(
        htmltools::tags$td("chr"),
        htmltools::tags$td(
          "Character strings, NA"
        )
      ),
      htmltools::tags$tr(
        htmltools::tags$td("fct"),
        htmltools::tags$td(
          "Categorical values"
        )
      ),
      htmltools::tags$tr(
        htmltools::tags$td("ord"),
        htmltools::tags$td(
          "Ordered categorial values"
        )
      )
    )
  )
}

help_type <- function(
  input, output, session, .values
) {
  
  ns <- session$ns
}