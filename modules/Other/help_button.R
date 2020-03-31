help_button <- function(inputId) {
  htmltools::span(
    class = "help-btn",
    m_action_button(
      inputId = inputId,
      label = NULL,
      icon = shiny::icon("question-circle")
    )
  )
}