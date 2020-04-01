init_constants <- function(.values) {
  .values$transformation$PREDICATES <- c(
    "select", "rename", "filter", "mutate", "group_by", "summarise", "plot"
  )
  
  .values$plot$GEOM <- tibble::tribble(
    ~name, ~class, ~n, ~optional,
    "area", "filled", 2, character(),
    "bin2d", "bin2d", 2, character(),
    "density2d", "filled", 2, character(),
    "hex", "filled", 2, character(),
    "line", "path", 2, character(),
    "path", "path", 2, character(),
    "point", "point", 2, character(),
    "smooth", "filled", 2, character(),
    "step", "path", 2, c("direction")
  )
  
  .values$plot$GEOM_CLASS_OPTIONAL_AES <- list(
    bin2d = c("alpha", "colour", "group", "linetype", "size"),
    filled = c("alpha", "colour", "fill", "group", "linetype", "size"),
    path = c("alpha", "colour", "group", "linetype", "size"),
    point = c("alpha", "colour", "fill", "group", "shape", "size")
  )
  
  .values$plot$REQUIRED_AES <- list(
    "x", c("x", "y"), c("x", "y", "z")
  )
  
  .values$plot$AES_CLASSES <- c(
    alpha = "percentage",
    colour = "colour",
    direction = "direction",
    fill = "colour",
    linetype = "linetype",
    shape = "shape",
    show.legend = "show.legend",
    size = "positive"
  )
  
  .values$plot$OPTIONAL_AES_NAMES <- c(
    "alpha", "colour", "direction", "fill", "group", "linetype", "shape",
    "show.legend", "size"
  )
  
  .values$plot$ALWAYS_OPTIONAL <- c(
    "show.legend"
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
    "fct", "factor", function(x) as.factor(unclass(x)), TRUE,
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