help_getting_started_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    htmltools::p(
      "Congratulations! You have just opened your first help page. These pages 
      explain the functionality and provide you with all information you need to 
      use as much features as possible. You will find the",
      shiny::icon("question-circle"),
      "icon everywhere in the app. If you click it, the corresponding help page
      will open. All help pages are located in separate tabs in the viewer. If
      the viewer gets too crowded, simply click the CLOSE ALL TABS button at the
      bottom.
      "
    ),
    htmltools::p(
      "This Getting Started Guide will take you to all the tabs. Let's start
      with the Data tab."
    ),
    shiny::actionLink(
      inputId = ns("goto_data"),
      label = "Click here to continue"
    )
  )
}

help_getting_started <- function(
  input, output, session, .values
) {
  
  ns <- session$ns
  
  shiny::observeEvent(input$goto_data, {
    .values$navbar$open("data")
    .values$help$open("gs_data")
  })
}