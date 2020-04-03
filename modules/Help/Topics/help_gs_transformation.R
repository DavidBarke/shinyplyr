help_gs_transformation_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    htmltools::p(
      "Welcome in the transformation tab. This tab contains the transformation
      table. At the first glance it looks quite simple, but beware: it's
      actually really powerful. To make sure, you don't miss anything, let's do
      our first transformation together. We will take the 'mpg' dataset from
      'ggplot2' for answering the following question: which manufacturer has the
      most distinct models based on this dataset?"
    ),
    htmltools::p(
      "Right now, the transformation table has only one row: the data operation. 
      Start your transformation by clicking on 'SELECT DATASET' and select the 
      'mpg' dataset, which is located in the 'ggplot2' folder. After you have 
      confirmed your selection, you will see that the text on the button has 
      changed to the name of our selected dataset."
    ),
    htmltools::p(
      "The transformation table consists of an arbitrary number of transformation
      rows. Each transformation row performs an operation on the dataset from the
      previous row. The details section is dependent on the operation. The button
      in the result section opens the result of this particular transformation row.
      Simply click it and you will see that the 'mpg' dataset opens up in the
      viewer."
    ),
    htmltools::h4("Select"),
    htmltools::p(
      "Let's add a new transformation row: click on the plus button
      at the bottom of the transformation table. Choose the 'select' operation
      and select the columns, we need to answer our initial question: 
      'manufacturer' and 'model'. Check your result by open the transformed
      data in the result section. That was pretty easy, right?"
    ),
    htmltools::h4("Rename"),
    htmltools::p(
      "How about changing the column names to upper case? Add a new transformation
      row and choose the 'rename' operation. Click the toggle button to open
      the rename table and enter 'Manufacturer' and 'Model'."
    ),
    htmltools::h4("Type"),
    htmltools::p(
      "As you might have noticed from the dataset table in the viewer, the columns
      'Manufacturer' and 'Model' are stored as characters. We should change them
      to factors. Add a new transformation row and choose the 'type' operation.
      Click the toggle button to open the type table and select 'fct' as new
      type for both columns."
    ),
    htmltools::h4("Group by"),
    htmltools::p(
      "To count the number of distinct models a manufacturer produces, we need
      to group by manufacturer. Add a new transformation row and choose the
      'group by' operation. Select 'Manufacturer' as grouping column."
    ),
    htmltools::h4("Summarise"),
    htmltools::p(
      "Now we are ready to summarise: add a new transformation and choose the
      'summarise' operation. Enter the name of the new column, e.g. 'n_distinct',
      choose the 'n_distinct' summary function and select the 'Model' column as
      input to the summary function. Open the result to answer our initial question."
    ),
    htmltools::h4("Plot"),
    htmltools::p(
      "Let's take our analysis to the next level: visualisation. Add a new 
      transformation row and choose the 'plot' operation. Select '2' as the number 
      of variables and open the plot in the result section. You will see a 
      simple ggplot2 with the point geometry, which is propably not the best possible 
      visualisation. Maybe we could create a bar plot with the col geometry. 
      Press the toggle button of the plot operation and create whatever plot you like."
    ),
    htmltools::h4("What's next?"),
    htmltools::p(
      "You finished the Getting Started Guide. Explorer the available datasets
      or import your own datasets using either the",
      shiny::actionLink(
        inputId = ns("nav_import_csv"),
        label = "csv"
      ),
      "or the",
      shiny::actionLink(
        inputId = ns("nav_import_rds"),
        label = "rds"
      ),
      "import."
    )
  )
}

help_gs_transformation <- function(
  input, output, session, .values
) {
  
  ns <- session$ns
  
  shiny::observeEvent(input$nav_import_csv, {
    .values$navbar$open("import_csv")
  })
  
  shiny::observeEvent(input$nav_import_rds, {
    .values$navbar$open("import_rds")
  })
}