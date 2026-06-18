#' Convert spatial objects to Last Mile Delivery zones input
#'
#' Zones specify a work territory as polygon features. The only recognized
#' attribute is `name`, which routes reference through their `zone_name`
#' attribute.
#'
#' @param x An `sf` or `sfc` polygon object delineating work territories,
#'   or `NULL`.
#' @param ... Additional arguments passed to methods.
#' @keywords internal
#' @export
as_lmd_zones <- function(x, ...) {
  UseMethod("as_lmd_zones")
}

#' @export
as_lmd_zones.NULL <- function(x, ...) {
  NULL
}

#' @export
as_lmd_zones.sfc <- function(x, ...) {
  arcgisutils::as_esri_featureset(x)
}

#' @export
as_lmd_zones.sf <- function(x, ...) {
  check_character(x[["name"]], arg = "name")

  x <- x["name"]
  names(x)[names(x) == "name"] <- "Name"

  arcgisutils::as_esri_featureset(x)
}
