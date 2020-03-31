help_filter_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    htmltools::p(
      "The filter operation defines a condition by which the dataset is filtered.
      For specifying multiple conditions you therefore need to create multiple
      transformation steps. Each condition is made up of three parts: a column,
      an operator and a value. The condition is composed as <column> <operator>
      <value>.
      "
    ),
    htmltools::h4(
      "Column"
    ),
    htmltools::p(
      "Select the name of a column that is present in the current dataset."
    ),
    htmltools::h4(
      "Operator"
    ),
    htmltools::p(
      "Select an operator. This operator is used for comparing each entry of
      the selected column against the selected value. The choices for the
      operator are dependent on the type of the selected column. For example it 
      is necessary to convert an integer to a factor using the type predicate in
      a previous transformation step if you need operators for factors."
    ),
    htmltools::h4(
      "Value"
    ),
    htmltools::p(
      "Enter a value. Choices for value are dependent on the selected operator."
    ),
    htmltools::h4(
      "Limitations"
    ),
    htmltools::tags$ul(
      htmltools::tags$li(
        "Multiple conditions need multiple transformation steps and are therefore
        always connected by the logical 'and' operator."
      ),
      htmltools::tags$li(
        "The value part of the condition can't be another column."
      )
    )
  )
}

help_filter <- function(
  input, output, session, .values
) {
  
  ns <- session$ns
}