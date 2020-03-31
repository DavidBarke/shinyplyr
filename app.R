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
        title = "Transformation",
        tab_transformation_ui(
          id = "id_tab_transformation"
        )
      ),
      shiny::tabPanel(
        title = "Import",
        tab_import_ui(
          id = "id_tab_import"
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
    .values$anim <- TRUE
    
    .values$dataset_id_rv <- shiny::reactiveVal(NULL)
    
    .values$transformation$PREDICATES <- c(
      "select", "rename", "filter", "mutate", "group_by", "summarise", "plot"
    )
    
    .values$plot$GEOM_NAMES <- c(
      "area", "bin2d", "density2d", "hex", "line", "path", "point",
      "smooth", "step"
    )
    
    .values$plot$OPTIONAL_AES_NAMES <- c(
      "alpha", "colour", "fill", "group", "linetype", "shape", "size"
    )
    
    .values$plot$REQUIRED_AES_NAMES <- c(
      "x", "y", "z"
    )
    
    .values$plot$AES_NAMES <- c(
      .values$plot$REQUIRED_AES_NAMES, .values$plot$OPTIONAL_AES_NAMES
    )
    
    .values$plot$LAYER <- tibble::tribble(
      ~layer, ~name,
      "aes", "Aesthetic",
      "geom", "Geometry",
      "facet", "Facets",
      "coord", "Coordinates",
      "theme", "Theme"
    )
    
    .values$help_rvs <- shiny::reactiveValues()
    
    shiny::isolate(
      fill_dataset_storage(.values$dataset_storage)
    )
    
    shiny::callModule(
      module = help_init,
      id = "id_help_init",
      .values = .values
    )
    
    shiny::callModule(
      module = tab_transformation,
      id = "id_tab_transformation",
      .values = .values 
    )
    
    shiny::callModule(
      module = tab_import,
      id = "id_tab_import",
      .values = .values
    )
    
  }
  
  list(ui = ui, server = server)
}

init <- init()

shinyApp(init$ui, init$server)