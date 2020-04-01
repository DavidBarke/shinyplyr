data_export_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::uiOutput(
    outputId = ns("export")
  )
}

data_export <- function(
  input, output, session, .values, data_r, name_r, row_index
) {
  
  ns <- session$ns
  
  output$export <- shiny::renderUI({
    shiny::req(data_r())
    shinyWidgets::dropdown(
      label = "Export",
      style = "material-flat",
      size = "xs",
      m_download_button(
        outputId = ns("export_rds"),
        label = "To .rds",
        style = "material-flat",
        color = "default"
      ),
      m_download_button(
        outputId = ns("export_xlsx"),
        label = "To .xlsx"
      ),
      m_download_button(
        outputId = ns("export_csv"),
        label = "To .csv"
      )
    )
  })
  
  base_name_r <- shiny::reactive({
    name_r() %_% row_index
  })
  
  output$export_rds <- shiny::downloadHandler(
    filename = function() {
      paste0(base_name_r(), ".rds")
    },
    content = function(file) {
      readr::write_rds(data_r(), file)
    }
  )
  
  output$export_xlsx <- shiny::downloadHandler(
    filename = function() {
      paste0(base_name_r(), ".xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(data_r(), file)
    }
  )
  
  output$export_csv <- shiny::downloadHandler(
    filename = function() {
      paste0(base_name_r(), ".csv")
    },
    content = function(file) {
      readr::write_csv(data_r(), file)
    }
  )
}