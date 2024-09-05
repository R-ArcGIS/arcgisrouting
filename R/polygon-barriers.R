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
as_polygon_barriers <- function(x, ...) {
  UseMethod("as_polygon_barriers")
}

#'@export
as_polygon_barriers.NULL <- function(x, ...) {
  NULL
}

#' @export
as_polygon_barriers.sfc <- function(x, ...) {

  if (!inherits(x, c("sfc_POLYGON", "sfc_MULTIPOLYGON"))) {
    cli::cli_abort("Polygon barriers must be a POLYGON or a MULTIPOLYGON not {obj_type_friendly(x)}")
  }

  if (is.na(sf::st_crs(x))) {
    cli::cli_abort(c("!" = "`crs` is not set. Please set the crs."))
  }

  if (length(x) > 2000) {
    cli::cli_abort("Only a maximum of 2000 polygon barriers can be provided found {.val {length(x)}}")
  }

  arcgisutils::as_esri_features(
    sf::st_sf(
      BarrierType = 0L,
      geometry = x
    )
  )

}


#'@export
as_polygon_barriers.sf <- function(x, ...) {

  if (is.na(sf::st_crs(x))) {
    cli::cli_abort(c("!" = "`crs` is not set. Please set the crs."))
  }

  if (ncol(x) == 1L) {
    return(as_polygon_barriers(sf::st_geometry(x)))
  }

  n <- nrow(x)
  if (n > 2000) {
    cli::cli_abort("Only a maximum of 2000 polygon barriers can be provided found {.val {n}}")
  }

  geo_type <- as.character(sf::st_geometry_type(x, by_geometry = FALSE))

  # check geometry type
  if (!geo_type %in% c("POLYGON", "MULTIPOLYGON")) {
    cli::cli_abort("Polygon barriers must be a POLYGON or a MULTIPOLYGON not {.val {geo_type}}")
  }

  # create a lookup for this here fellas
  imp_lu <- setNames(paste0("Attr_", impedance_lu), names(impedance_lu))
  lu <- c(imp_lu, "name" = "Name")

  common_cols <- intersect(names(lu), colnames(x))
  
  if (is.null(common_cols)) {
    as_polygon_barriers(sf::st_geometry(x))
  }

  # subset in the event there are extras
  x <- x[common_cols]

  # do column level checks in here
  for (col in common_cols) {
    if (col == "name") {
      check_character(x[[col]])
    } else if (!rlang::is_bare_numeric(x[[col]])) {
      cli::cli_abort("Expected impedance column {.col {col}} to be numeric. Found {obj_type_friendly(x[[col]])}.")
    }
  }
  

  # set the names appropriately
  colnames(x) <- c(lu[common_cols], "geometry")
  sf::st_geometry(x) <- "geometry"

  # set the barrier type
  x[["BarrierType"]] <- 1L

  arcgisutils::as_esri_features(x)
}