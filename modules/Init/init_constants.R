init_constants <- function(.values) {
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
  
  .values$ANIM <- TRUE
  
  .values$TYPE_DATA <- tibble::tribble(
    ~type, ~name, ~convert_fun, ~allowed,
    "lgl", "logical", as.logical, TRUE,
    "int", "integer", as.integer, TRUE,
    "dbl", "double", as.double, TRUE,
    "chr", "character", as.character, TRUE,
    "cpl", "complex", as.complex, TRUE,
    "raw", "raw", as.raw, FALSE,
    "list", "list", as.list, FALSE,
    "named list", "named list", as.list, FALSE,
    "fct", "factor", as.factor, TRUE,
    "ord", "ordered", as.ordered, TRUE,
    "date", "Date", as.Date, FALSE,
    "dttm", "POSIXt", as.POSIXct, FALSE,
    "drtn", "difftime", vctrs::new_duration, FALSE,
    "time", "hms", hms::as_hms, FALSE,
    "int64", "integer64", bit64::as.integer64, FALSE,
    "blob", "blob", blob::as.blob, FALSE,
    "df[,1]", "data.frame", as.data.frame, FALSE,
    "tibble", "tbl_df", tibble::as_tibble, FALSE
  )
}