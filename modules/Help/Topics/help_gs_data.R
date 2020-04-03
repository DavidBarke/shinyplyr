help_gs_data_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    htmltools::p(
      "Now we are in the Data tab. If you want to return to the first page of
      the Getting Started Guide, simply click the",
      shiny::icon("arrow-left"),
      "button right of the title of this page."
    ),
    htmltools::p(
      "The Data tab shows the Dataset Explorer. Its folders contain all the datasets
      we will work with in the transformation tab. Double-click on the 'Package
      datasets' folder. As you can see, there are folders for the packages
      'datasets', 'dplyr' and 'ggplot2'. Each folder contains all the datasets 
      exported by that package. To see their contents, double-click on one
      of them again. Alternatively, you can right-click and choose 'Open' from the
      contextmenu. Right-clicking a dataset and choosing 'Open' will open the
      dataset in the viewer."
    ),
    htmltools::p(
      "The 'Imported datasets' folder is still empty. You can fill it with custom
      datasets by using one of the Import tabs."
    ),
    htmltools::p(
      "Now that you are familiar with the Dataset Explorer, we can get to the
      heart of this application: the Transformation Table."
    ),
    shiny::actionLink(
      inputId = ns("goto_transformation"),
      label = "Click here to continue"
    )
  )
}

help_gs_data <- function(
  input, output, session, .values
) {
  
  ns <- session$ns
  
  shiny::observeEvent(input$goto_transformation, {
    .values$navbar$open("transformation")
    .values$help$open("gs_transformation")
  })
}