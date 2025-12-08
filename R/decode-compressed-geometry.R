#' Decode Compressed Geometry
#'
#' Decodes ArcGIS compressed geometry strings and converts them to an sfc object.
#'
#' @param geometry Character vector containing compressed geometry strings.
#' @returns An `sfc` object with `MULTIPOINT` geometries in EPSG:4326.
#' @export
#' @references [maslke/arcgis-compressed-geometry](https://github.com/maslke/arcgis-compressed-geometry)
decode_compressed_geometry <- function(geometry) {
  check_character(geometry)
  res <- .Call(decode_compressed_geometry_, geometry)

  for (i in seq_along(res)) {
    res[[i]] <- sf::st_multipoint(res[[i]])
  }

  sf::st_sfc(res, crs = 4326)
}
