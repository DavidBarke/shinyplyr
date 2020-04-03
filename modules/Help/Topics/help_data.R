help_data_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    htmltools::p(
      "The",
      shiny::actionLink(
        inputId = ns("nav_data"),
        label = "data tab"
      ),
      "contains the dataset explorer. Each item represents either
      a folder or a dataset. Right-click an item to open its contextmenu:
      Folders can be added, renamed or deleted (both predefined folders can't be
      deleted), datasets can be opened in the viewer or renamed. Double-click a 
      folder to open its contents. Use the navigation head for quick navigation. 
      A click on", shiny::icon("angle-right"), "opens a menu with links to the 
      children of the corresponding item."
    )
  )
}

help_data <- function(
  input, output, session, .values
) {
  
  ns <- session$ns
  
  shiny::observeEvent(input$nav_data, {
    .values$navbar$open("data")
  })
}