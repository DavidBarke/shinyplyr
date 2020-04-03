geom_class <- function(geom, .values) {
  if (!geom %in% .values$plot$GEOM$name) print(paste(geom, "is missing in .values$plot$GEOM"))
  filter(.values$plot$GEOM, name == !!geom)$class
}