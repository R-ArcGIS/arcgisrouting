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



#' @export
as_point_barriers <- function(x, locate_on_barriers = FALSE, ...) {
  UseMethod("as_point_barriers")
}

#' @export
as_point_barriers.sfc <- function(x, ...) {
  # if an sfc, then we just assume all points are barriers
  if (is.na(sf::st_crs(x))) {
    cli::cli_abort(c("!" = "`crs` is not set. Please set the crs."))
  }

  arcgisutils::as_esri_features(
    sf::st_sf(
      BarrierType = 0,
      geometry = x
    )
  )

}


#'@export
as_point_barriers.sf <- function(x, ...) {
  if (is.na(sf::st_crs(x))) {
    cli::cli_abort(c("!" = "`crs` is not set. Please set the crs."))
  }

  if (ncol(x) == 1L) {
    NextMethod()
  }

  # create a lookup for this here fellas
  lu <- c(impedance_lu, "name" = "Name", "full_edge" = "FullEdge")

  common_cols <- intersect(names(lu), colnames(x))
  
  if (is.null(common_cols)) {
    NextMethod()
  }

  # subset in the event there are extras
  x <- x[common_cols]

  # do column level checks in here
  for (col in common_cols) {
    # if the column is full_edge check that it is a boolean and coerce to int
    if (col == "full_edge") {
      check_logical(x[[col]], arg = col)
      x[[col]] <- as.integer(x[[col]])
    } else if (col == "name") {
      check_character(x[[col]])
    } else if (!rlang::is_bare_numeric(x[[col]])) {
      cli::cli_abort("Expected impedance column {.col {col}} to be numeric. Found {obj_type_friendly(x[[col]])}.")
    }
  }

  # set the names appropriately
  colnames(x) <- c(lu[common_cols], "geometry")
  sf::st_geometry(x) <- "geometry"

  # set the barrier type
  x[["BarrierType"]] <- 2L

  arcgisutils::as_esri_features(x)
}