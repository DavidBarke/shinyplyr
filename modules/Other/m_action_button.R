m_action_button <- function(
  inputId, label, icon = NULL, style = "material-flat", color = "default",
  size = "xs", block = FALSE, no_outline = TRUE, tooltip = NULL, ...,
  dropdown = FALSE
) {
  if (dropdown) {
    ui <- div(
      style = "margin: 0px 2px",
      shinyWidgets::actionBttn(
        inputId = inputId,
        label = label,
        icon = icon,
        style = style,
        color = color,
        size = size,
        block = block,
        no_outline = no_outline
      )
    )
  } else {
    ui <- shinyWidgets::actionBttn(
      inputId = inputId,
      label = label,
      icon = icon,
      style = style,
      color = color,
      size = size,
      block = block,
      no_outline = no_outline
    )
  }
  
  if (!is.null(tooltip)) {
    ui <- tagList(
      ui,
      shinyBS::bsTooltip(
        id = inputId,
        title = tooltip,
        placement = "top"
      )
    )
  }
  
  ui
}