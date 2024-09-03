# maximum 250 points
# can have `doNotLocateOnRestrictedElements` property

#' BarrierType: 
#'   0 (restriction)
#'   2 (added cost)
#' Must have accompanying attribute 
#' Valid attributes Attr_TravelTime , Attr_Miles , Attr_Kilometers , Attr_Minutes , Attr_WalkTime , Attr_TruckMinutes , or Attr_TruckTravelTime
# ObjectID 
# FullEdge
# preserveObjectId: we will ignore this for now unless specifically asked for.

# accept sfc_POINT object - all are restrictionss
# sf object with 


# x <- od::od_data_network$geometry[1:200]
# x <- od::od_data_network[1:200,]
# x[["name"]] <- sample(letters, 200, TRUE)

#' @export
as_polyline_barriers <- function(x, ...) {
  UseMethod("as_polyline_barriers")
}

#'@export
as_polyline_barriers.NULL <- function(x, ...) {
  NULL
}

#' @export
as_polyline_barriers.sfc <- function(x, ...) {

  if (!inherits(x, c("sfc_LINESTRING", "sfc_MULTILINESTRING"))) {
    cli::cli_abort("Polyline barriers must be a LINESTRING or a MULTILINESTRING not {obj_type_friendly(x)}")
  }

  # if an sfc, then we just assume all points are barriers
  if (is.na(sf::st_crs(x))) {
    cli::cli_abort(c("!" = "`crs` is not set. Please set the crs."))
  }

  if (length(x) > 500) {
    cli::cli_abort("Only a maximum of 500 polyline barriers can be provided found {.val {length(x)}}")
  }

  arcgisutils::as_esri_features(x)

}


#'@export
as_polyline_barriers.sf <- function(x, ...) {

  if (is.na(sf::st_crs(x))) {
    cli::cli_abort(c("!" = "`crs` is not set. Please set the crs."))
  }

  if (ncol(x) == 1L) {
    return(as_polyline_barriers(sf::st_geometry(x)))
  }

  n <- nrow(x)
  if (n > 500) {
    cli::cli_abort("Only a maximum of 500 polyline barriers can be provided found {.val {n}}")
  }

  geo_type <- as.character(sf::st_geometry_type(x, by_geometry = FALSE))

  # check geometry type
  if (!geo_type %in% c("LINESTRING", "MULTILINESTRING")) {
    cli::cli_abort("Polyline barriers must be a LINESTRING or a MULTILINESTRING not {.val {geo_type}}")
  }

  # we check if there is a name field
  if (is.null(x[["name"]])) {
    return(as_polyline_barriers(sf::st_geometry(x)))
  } else {

    x <- sf::st_sf(
      Name = x[["name"]],
      x = sf::st_geometry(x)
    )

    return(arcgisutils::as_esri_features(x))
  }
}