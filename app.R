library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(colourpicker)

library(R.utils)

library(purrr)
library(dplyr)
library(stringr)

library(ggplot2)

library(readr)
library(writexl)

# install with remotes::install_github("DavidBarke/shinyExplorer")
library(shinyExplorer)

# Source files inside of function, so that globalenv doesn't get polluted
init <- function() {
  # Initialisation -------------------------------------------------------------
  source("./modules/Other/source_directory.R", encoding = "UTF-8", chdir = T)
  
  source_directory(
    # chdir makes it possible to use relative paths in source statements inside
    # sourced files
    path = "./modules", encoding = "UTF-8", modifiedOnly = FALSE, 
    chdir = TRUE, verbose = FALSE, recursive = TRUE
  )
  
  options(DT.options = list(dom = "lfptp", scrollX = TRUE))
  
  
  # UI -------------------------------------------------------------------------
  ui <- htmltools::div(
    shinyWidgets::setBackgroundColor(),
    shinyWidgets::useShinydashboard(),
    htmltools::tags$head(
      shiny::includeCSS("www/css/styles.css")
    ),
    shiny::navbarPage(
      id = "navbar",
      title = "shinyplyr",
      shiny::tabPanel(
        title = "Home",
        value = "home",
        tab_home_ui(
          id = "id_tab_home"
        )
      ),
      shiny::tabPanel(
        title = "Data",
        value = "data",
        tab_data_ui(
          id = "id_tab_data"
        )
      ),
      shiny::tabPanel(
        title = "Transformation",
        value = "transformation",
        tab_transformation_ui(
          id = "id_tab_transformation"
        )
      ),
      shiny::navbarMenu(
        title = "Import",
        shiny::tabPanel(
          title = "csv",
          value = "import_csv",
          tab_csv_import_ui(
            id = "id_tab_csv_import"
          )
        ),
        shiny::tabPanel(
          title = "rds",
          value = "import_rds", 
          tab_rds_import_ui(
            id = "id_tab_rds_import"
          )
        )
      )
    ),
    shiny::fluidPage(
      viewer_ui(
        id = "id_viewer"
      )
    ),
    shinyjs::useShinyjs()
  )
  
  
  # Server ---------------------------------------------------------------------
  server <- function(input, output, session) {
    
    # Create an environment that is passed to every module. 
    .values <- new.env()
    
    # Init dataset storage as shinyExplorer tree
    .values$explorer_classes <- init_explorer_classes()
    shiny::isolate({
      .values$tree <- shinyExplorer::ExplorerTree$new(
        root_object = Object$new("home")
      )
      init_tree(.values$tree, .values)
    })
    
    .values$navbar <- list(
      session = session,
      id = "navbar",
      open = function(value) {
        shiny::updateNavbarPage(
          session = .values$navbar$session,
          inputId = .values$navbar$id,
          selected = value
        )
      }
    )
    
    init_constants(.values)
    init_reactive_vals(.values)
    
    shiny::callModule(
      module = help_init,
      id = "id_help_init",
      .values = .values
    )
    
    shiny::callModule(
      module = tab_home,
      id = "id_tab_home",
      .values = .values
    )
    
    shiny::callModule(
      module = tab_data,
      id = "id_tab_data",
      .values = .values
    )
    
    shiny::callModule(
      module = tab_transformation,
      id = "id_tab_transformation",
      .values = .values 
    )
    
    shiny::callModule(
      module = tab_csv_import,
      id = "id_tab_csv_import",
      .values = .values
    )
    
    shiny::callModule(
      module = tab_rds_import,
      id = "id_tab_rds_import",
      .values = .values
    )
    
    shiny::callModule(
      module = viewer,
      id = "id_viewer",
      .values = .values
    )
    
  }
  
  list(ui = ui, server = server)
}

init <- init()

shinyApp(init$ui, init$server)