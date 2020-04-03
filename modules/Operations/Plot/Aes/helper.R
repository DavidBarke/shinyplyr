all_aes <- function(geom, n, .values) {
  list(
    required = if (n == 1) "x" else c("x", "y"), 
    optional_class = .values$plot$GEOM_CLASS_OPTIONAL_AES[[geom_class(geom, .values)]],
    optional_geom = c(
      filter(.values$plot$GEOM, name == !!geom)$optional[[1]],
      .values$plot$ALWAYS_OPTIONAL
    )
  )
}

aes_class <- function(aes, .values) {
  if (!aes %in% names(.values$plot$AES_CLASSES)) print(paste(aes, "is missing in .values$plot$AES_CLASSES"))
  .values$plot$AES_CLASSES[aes]  
}

helper_aes_geom_allowed <- function(geom, .values) {
  row <- dplyr::filter(.values$plot$GEOM, name == !!geom)
  c(x = row$allowed_x, y = row$allowed_y)
}