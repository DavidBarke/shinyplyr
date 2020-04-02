init_constants <- function(.values) {
  .values$transformation$PREDICATES <- c(
    "select", "rename", "filter", "mutate", "group_by", "summarise", "plot"
  )
  
  .values$plot$GEOM <- tibble::tribble(
    ~name, ~class, ~optional,
    "area", "filled", character(),
    "bar", "filled", character(),
    "bin2d", "bin2d", "bins",
    "density2d", "filled", character(),
    "dotplot", "dotplot", character(),
    "hex", "filled", "bins",
    "histogram", "filled", "bins",
    "line", "path", character(),
    "path", "path", character(),
    "point", "point", character(),
    "smooth", "filled", character(),
    "step", "path", "direction"
  )
  
  .values$plot$GEOM_CLASS_OPTIONAL_AES <- list(
    bin2d = c("alpha", "colour", "group", "linetype", "size"),
    dotplot = c("alpha", "colour", "fill", "group", "linetype"),
    filled = c("alpha", "colour", "fill", "group", "linetype", "size"),
    path = c("alpha", "colour", "group", "linetype", "size"),
    point = c("alpha", "colour", "fill", "group", "shape", "size")
  )
  
  .values$plot$REQUIRED_AES <- list(
    "x", c("x", "y"), c("x", "y", "z")
  )
  
  .values$plot$AES_CLASSES <- c(
    alpha = "percentage",
    bins = "positive",
    colour = "colour",
    direction = "direction",
    fill = "colour",
    linetype = "linetype",
    shape = "shape",
    show.legend = "show.legend",
    size = "positive"
  )
  
  .values$plot$OPTIONAL_AES_NAMES <- c(
    "alpha", "bins", "colour", "direction", "fill", "group", "linetype", "shape",
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
    ~type, ~name, ~convert_fun, ~allowed, ~filter,
    "lgl", "logical", as.logical, TRUE, "logical",
    "int", "integer", as.integer, TRUE, "numeric",
    "dbl", "double", as.double, TRUE, "numeric",
    "chr", "character", as.character, TRUE, "character",
    "cpl", "complex", as.complex, TRUE, "missing",
    "raw", "raw", as.raw, FALSE, "missing",
    "list", "list", as.list, FALSE, "missing",
    "named list", "named list", as.list, FALSE, "missing",
    "fct", "factor", function(x) as.factor(unclass(x)), TRUE, "factor",
    "ord", "ordered", as.ordered, TRUE, "factor",
    "date", "Date", as.Date, FALSE, "date",
    "dttm", "POSIXt", as.POSIXct, FALSE, "missing",
    "drtn", "difftime", vctrs::new_duration, FALSE, "missing",
    "time", "hms", hms::as_hms, FALSE, "missing",
    "int64", "integer64", bit64::as.integer64, FALSE, "missing",
    "blob", "blob", blob::as.blob, FALSE, "missing",
    "df[,1]", "data.frame", as.data.frame, FALSE, "missing",
    "tibble", "tbl_df", tibble::as_tibble, FALSE, "missing"
  )
  
  .values$FILTER_OPERATORS <- list(
    "character" = c("=" = "eq", "in" = "in", "~" = "re"),
    "date" = c("=" = "eq", "zwischen" = "bw"),
    "factor" = c("=" = "eq", "in" = "in"),
    "logical" = c("=" = "eq", "!=" = "ne"),
    "numeric" = c("=" = "eq", "<=" = "le", ">=" = "ge", "<" = "lt", ">" = "gt", "!=" = "ne", "zwischen" = "bw")
  )
  
  .values$SUMMARISE_FUN <- tibble::tribble(
    ~name, ~fun, ~allowed,
    "mean", base::mean, "numeric",
    "median", stats::median, "numeric",
    "sd", stats::sd, "numeric",
    "IQR", stats::IQR, "numeric",
    "mad", stats::mad, "numeric",
    "min", base::min, c("numeric", "date", "factor"),
    "max", base::max, c("numeric", "date", "factor"),
    "first", dplyr::first, "__ALL__",
    "last", dplyr::last, "__ALL__",
    "n", dplyr::n, "__ALL__",
    "n_distinct", dplyr::n_distinct, "__ALL__",
    "any", base::any, "logical",
    "all", base::all, "logical"
  )
}