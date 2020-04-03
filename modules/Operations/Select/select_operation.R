select_operation_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::div(
    class = "select-op-container grid-gap",
    # Grid layout
    htmltools::div(
      shiny::selectInput(
        inputId = ns("select_type"),
        label = NULL,
        choices = list(
          "Selection Helper" = list(
            "=" = "eq",
            starts_with = "s_w",
            ends_with = "e_w",
            contains = "contains",
            matches = "matches"
          )
        )
      )
    ),
    htmltools::div(
      shiny::uiOutput(
        outputId = ns("select_val")
      )
    )
  )
}

select_operation <- function(
  input, output, session, .values, data_r
) {
  
  ns <- session$ns
  
  choices_r <- shiny::reactive({
    names(data_r())
  })
  
  output$select_val <- shiny::renderUI({
    if (shiny::req(input$select_type) == "eq") {
      eq_val_ui_r()
    } else {
      text_val_ui_r()
    }
  })
  
  eq_val_ui_r <- shiny::reactive({
    shiny::selectInput(
      inputId = ns("eq_val"),
      label = NULL,
      choices = choices_r(),
      selected = choices_r(),
      multiple = TRUE
    )
  })
  
  text_val_ui_r <- shiny::reactive({
    htmltools::div(
      class = "fifty-fifty-grid",
      htmltools::div(
        class = "adjust-form-control-height",
        shiny::textInput(
          inputId = ns("text_val"),
          label = NULL,
          placeholder = paste("e.g.", choices_r()[1])
        )
      ),
      htmltools::div(
        shiny::uiOutput(
          outputId = ns("matched_columns"),
          inline = TRUE
        )
      )
    )
  })
  
  output$matched_columns <- shiny::renderUI({
    shinyjs::disabled(
      shiny::selectInput(
        inputId = ns("matched_columns"),
        label = NULL,
        choices = selected_names_r(),
        selected = selected_names_r(),
        multiple = TRUE
      )
    )
  })
  
  select_fun_r <- shiny::reactive({
    switch(
      shiny::req(input$select_type),
      "eq" = tidyselect::all_of,
      "s_w" = tidyselect::starts_with,
      "e_w" = tidyselect::ends_with,
      "contains" = tidyselect::contains,
      "matches" = tidyselect::matches
    )
  })
  
  select_val_r <- shiny::reactive({
    if (shiny::req(input$select_type) == "eq") {
      shiny::req(input$eq_val)
    } else {
      shiny::req(input$text_val)
    }
  })
  
  selected_data_r <- shiny::reactive({
    tryCatch(
      {
        data_r() %>%
          select(select_fun_r()(select_val_r()))
      },
      error = function(e) data_r()
    )
  })
  
  selected_names_r <- shiny::reactive({
    names(selected_data_r())
  })
  
  return_list <- list(
    data_r = selected_data_r
  )
  
  return(return_list)
}