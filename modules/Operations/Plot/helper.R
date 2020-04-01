geom_class <- function(geom, .values) {
  if (!geom %in% .values$plot$GEOM$name) print(paste(geom, "is missing in .values$plot$GEOM"))
  filter(.values$plot$GEOM, name == !!geom)$class
}

all_aes <- function(geom, n, .values) {
  list(
    required = .values$plot$REQUIRED_AES[[n]], 
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