m_action_button <- function(
  inputId, label, icon = NULL, style = "material-flat", color = "default",
  size = "xs", block = FALSE, no_outline = TRUE, ...,
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
  
  ui
}

m_download_button <- function(
  outputId, label = "Download", style = "material-flat", color = "default",
  size = "xs", block = FALSE, no_outline = TRUE
) {
  shinyWidgets::downloadBttn(
    outputId,
    label,
    style,
    color,
    size,
    block,
    no_outline
  )
}