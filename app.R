library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)

library(R.utils)

library(purrr)
library(dplyr)
library(ggplot2)

# Source files inside of function, so that globalenv doesn't get polluted
init <- function() {
  # Initialisation ---------------------------------------------------------------
  source("./modules/Other/source_directory.R", encoding = "UTF-8", chdir = T)
  
  source_directory(
    # chdir makes it possible to use relative paths in source statements inside
    # sourced files
    path = "./modules", encoding = "UTF-8", modifiedOnly = FALSE, 
    chdir = TRUE, verbose = TRUE, recursive = TRUE
  )
  
  options(DT.options = list(dom = "lfptp", scrollX = TRUE))
  
  
  # UI ---------------------------------------------------------------------------
  ui <- htmltools::div(
    shinyWidgets::setBackgroundColor(),
    shinyWidgets::useShinydashboard(),
    htmltools::tags$head(
      shiny::includeCSS("www/css/styles.css")
    ),
    shiny::navbarPage(
      title = "shinyplyr",
      shiny::tabPanel(
        title = "Home",
        tab_home_ui(
          id = "id_tab_home"
        )
      ),
      shiny::tabPanel(
        title = "About"
      )
    ),
    shinyjs::useShinyjs()
  )
  
  
  # Server -----------------------------------------------------------------------
  server <- function(input, output, session) {
    
    # Create an environment that is passed to every module. 
    .values <- new.env()
    
    .values$dataset_storage = ObjectStorage$new("DatasetObject")
    
    shiny::isolate(
      fill_dataset_storage(.values$dataset_storage)
    )
    
    shiny::callModule(
      module = tab_home,
      id = "id_tab_home",
      .values = .values
    )
    
  }
  
  list(ui = ui, server = server)
}

init <- init()

shinyApp(init$ui, init$server)