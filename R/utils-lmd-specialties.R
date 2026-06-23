# Shared converter for the order/route specialty long-format tables. Each maps
# a foreign-key name column and a specialty name column to the API fields.
as_specialties <- function(x, key_snake, key_api, arg) {
  lu <- stats::setNames(
    c(key_api, "SpecialtyName"),
    c(key_snake, "specialty_name")
  )

  common_cols <- intersect(names(lu), colnames(x))

  if (length(common_cols) == 0L) {
    cli::cli_abort(
      c(
        "No recognized columns found in {.arg {arg}}",
        "i" = "Recognized columns: {.val {names(lu)}}"
      )
    )
  }

  for (col in common_cols) {
    check_character(x[[col]], arg = col)
  }

  x <- x[common_cols]
  colnames(x) <- unname(lu[common_cols])

  arcgisutils::as_esri_featureset(x)
}

#' Convert a data.frame to Last Mile Delivery order specialties input
#'
#' @param x A `data.frame` with columns `order_name` and `specialty_name`,
#'   or `NULL`.
#' @param ... Additional arguments passed to methods.
#' @keywords internal
as_order_specialties <- function(x, ...) {
  UseMethod("as_order_specialties")
}

#' @export
as_order_specialties.NULL <- function(x, ...) {
  NULL
}

#' @export
as_order_specialties.data.frame <- function(x, ...) {
  as_specialties(x, "order_name", "OrderName", "order_specialties")
}

#' Convert a data.frame to Last Mile Delivery route specialties input
#'
#' @param x A `data.frame` with columns `route_name` and `specialty_name`,
#'   or `NULL`.
#' @param ... Additional arguments passed to methods.
#' @keywords internal
#' @export
as_route_specialties <- function(x, ...) {
  UseMethod("as_route_specialties")
}

#' @export
as_route_specialties.NULL <- function(x, ...) {
  NULL
}

#' @export
as_route_specialties.data.frame <- function(x, ...) {
  as_specialties(x, "route_name", "RouteName", "route_specialties")
}
