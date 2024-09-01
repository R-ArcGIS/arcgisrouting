# This function will convert a number of different representations
# into points that can be used instead of sfc
obj_as_points <- function(
    x,
    allow_null = TRUE,
    arg = rlang::caller_arg(x),
    call = rlang::caller_env()) {
  if (is.null(x) && allow_null) {
    return(NULL)
  } else if (rlang::inherits_any(x, "POINT")) {
    if (abs(x[1]) > 180 || abs(x[[2]]) > 90) {
      abort_4326(arg, call)
    }
    return(sf::st_sfc(x, crs = 4326))
  } else if (rlang::inherits_any(x, "matrix") && is.numeric(x)) {
    if (max(abs(x[, 1]), na.rm = TRUE) > 180 || max(abs(x[, 2]), na.rm = TRUE) > 90) {
      abort_4326(arg, call)
    }
    return(sf::st_cast(sf::st_sfc(sf::st_multipoint(x), crs = 4326), "POINT"))
  } else if (is.numeric(x)) {
    if (abs(x[1]) > 180 || abs(x[[2]]) > 90) {
      abort_4326(arg, call)
    } else if (length(x) > 4) {
      cli::cli_abort("{arg} is {obj_type_friendly(x)} and cannot exceed 4 elements", call = call)
    }
    return(sf::st_sfc(sf::st_point(x), crs = 4326))
  } else if (rlang::inherits_all(x, c("sfc_POINT", "sfc"))) {
    return(x)
  } else {
    cli::cli_abort(c(
      "{.arg {arg}} cannot be converted to a point",
      "i" = "found {obj_type_friendly(x)}"
    ), arg = arg, call = call)
  }
}

abort_4326 <- function(arg, call) {
  cli::cli_abort(c(
    "{.arg {arg}}, {obj_type_friendly(x)}, must be in EPSG:4326",
    ">" = "{.code longitude} values must be in the range [-180, 180]",
    " " = "and {.code latitude} values must be in the range [-90, 90]"
  ), call = call)
}

#' Add `tbl` class to a data.frame
#'
#' When pillar is loaded, a data.frame will print like a tibble
#' but will not inherit the tibble class.
#'
#' @noRd
#' @keywords internal
data_frame <- function(x, call = rlang::caller_env()) {
  check_data_frame(x, call = call)
  structure(x, class = c("tbl", "data.frame"))
}
