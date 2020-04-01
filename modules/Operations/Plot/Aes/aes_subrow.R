aes_subrow_ui <- function(id, row_index) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    htmltools::div(
      id = ns("aes_subrow"),
      class = "plot-subrow-content aes-subrows-open",
      shiny::uiOutput(
        outputId = ns("index"),
        class = "grid-center"
      ),
      shiny::uiOutput(
        outputId = ns("content"),
        class = "grid-gap aes-content"
      )
    ),
    shiny::uiOutput(
      outputId = ns("subrows"),
      class = "subrows-container"
    )
  )
}

aes_subrow <- function(
  input, output, session, .values, data_r, row_index, geom_r, n_var_r
) {
  
  ns <- session$ns
  
  aes_return_env <- new.env()
  
  choices_r <- shiny::reactive({
    names(data_r())
  })
  
  purrr::walk2(.values$plot$AES_NAMES, seq_along(.values$plot$AES_NAMES), function(aes, index) {
    if (aes %in% .values$plot$REQUIRED_AES_NAMES) {
      selected_r <- shiny::reactive(choices_r()[index])
      .choices_r <- shiny::reactive(list("Select a column" = as.list(choices_r())))
    } else {
      selected_r <- shiny::reactive("NULL")
      .choices_r <- shiny::reactive({
        list("Select a column or NULL" = as.list(c("NULL", choices_r())))
      })
    }
    
    aes_return_env[[aes]] <- shiny::callModule(
      module = aes_subsubrow,
      id = aes %_% "subsubrow",
      .values = .values,
      choices_r = .choices_r,
      selected_r = selected_r
    )
  })
  
  # Toggle aes subrows ---------------------------------------------------------
  output$subrows_toggle_btn <- shiny::renderUI({
    m_action_button(
      inputId = ns("sr_toggle"),
      label = NULL,
      icon = toggled_icon_r()
    )
  })
  
  toggle_rv <- shiny::reactiveVal(0)
  
  toggled_icon_r <- shiny::reactive({
    if (toggle_rv() %% 2 == 0) {
      shiny::icon("caret-down")
    } else {
      shiny::icon("caret-right")
    }
  })
  
  shiny::observeEvent(input$sr_toggle, {
    toggle_rv(toggle_rv() + 1)
  })
  
  aes_subrows_selector <- paste0("#", ns("subrows"))
  aes_subrow_container_selector <-  paste0("#", ns("aes_subrow"))
  
  shiny::observeEvent(toggle_rv(), {
    if (toggle_rv() %% 2 == 0) {
      shinyjs::show(
        anim = .values$ANIM,
        selector = aes_subrows_selector
      )
      
      shinyjs::addClass(
        class = "aes-subrows-open",
        selector = aes_subrow_container_selector
      )
    } else {
      shinyjs::hide(
        anim = .values$ANIM,
        selector = aes_subrows_selector
      )
      
      shinyjs::removeClass(
        class = "aes-subrows-open",
        selector = aes_subrow_container_selector
      )
    }
  })
  
  # Outputs --------------------------------------------------------------------
  subrow_index <- paste(row_index, 1, sep = ".")
  
  output$index <- shiny::renderUI({
    subrow_index
  })
  
  all_aes_r <- shiny::reactive({
    all_aes(geom_r(), n_var_r(), .values)
  })
  
  aes_names_r <- shiny::reactive({
    unlist(all_aes_r())
  })
  
  req_opt_names_r <- shiny::reactive({
    unlist(all_aes_r()[c("required", "optional_class")])
  })
  
  output$subrows <- shiny::renderUI({
    purrr::map2(req_opt_names_r(), seq_along(req_opt_names_r()), function(aes, index) {
      htmltools::div(
        class = "subrow-container grid-gap m-index",
        htmltools::div(
          class = "subrow-index grid-center",
          paste(subrow_index, index, sep = ".")
        ),
        htmltools::div(
          class = "subrow-content aes-subrow-content grid-gap",
          htmltools::div(
            class = "grid-vertical-center",
            aes
          ),
          aes_subsubrow_ui(
            id = ns(aes %_% "subsubrow")
          )
        ),
        htmltools::div(
          class = "subrow-remove"
        )
      )
    })
  })
  
  output$content <- shiny::renderUI({
    ui <- if (toggle_rv() %% 2 == 0) {
      htmltools::tagList(
        htmltools::div(
          class = "aes-title-help grid-gap",
          htmltools::tags$b(
            class = "grid-vertical-center",
            "Aesthetic"
          ),
          htmltools::div(
            class = "grid-center",
            help_button(ns("help_plot_aes"))
          )
        ),
        htmltools::tags$b(
          class = "grid-vertical-center",
          "Column"
        )
      )
    } else {
      aes_name_val <- purrr::map2_chr(req_opt_names_r(), selected_aes_vals_r(), function(aes, val) {
        paste(aes, val, sep = ": ")
      })
      
      htmltools::tagList(
        htmltools::div(
          class = "aes-title-help grid-gap",
          htmltools::tags$b(
            class = "grid-vertical-center",
            "Aesthetic"
          ),
          htmltools::div(
            class = "grid-center",
            help_button(ns("help_plot_aes"))
          )
        ),
        htmltools::div(
          class = "grid-vertical-center",
          paste(aes_name_val, collapse = ", ")
        )
      )
    }
    
    htmltools::tagList(
      htmltools::div(
        shiny::uiOutput(
          outputId = ns("subrows_toggle_btn"),
          class = "sr-toggle-btn"
        )
      ),
      ui
    )
  })
  
  selected_aes_vals_r <- shiny::reactive({
    purrr::map_chr(req_opt_names_r(), function(aes) {
      aes_return_env[[aes]]$value_r()
    })
  })
  
  non_null_aes_names_r <- shiny::reactive({
    x <- req_opt_names_r()[selected_aes_vals_r() != "NULL"]
    setNames(x, x)
  })
  
  free_aes_names_r <- shiny::reactive({
    setdiff(
      union(
        req_opt_names_r()[selected_aes_vals_r() == "NULL"],
        all_aes_r()$optional_geom
      ),
      c("group")
    )
  })
  
  aes_r <- shiny::reactive({
    aes_list <- purrr::map(non_null_aes_names_r(), function(aes) {
      aes_return_env[[aes]]$value_r()
    })
    
    do.call(ggplot2::aes_string, aes_list)
  })
  
  shiny::observeEvent(input$help_plot_aes, {
    .values$help$open("plot_aes")
  })
  
  return_list <- list(
    aes_r = aes_r,
    free_aesthetics_r = free_aes_names_r
  )
  
  return(return_list)
}