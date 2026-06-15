#' Convert spatial objects to VRP orders input
#'
#' @param x An `sfc` or `sf` object containing point geometries.
#' @param ... Additional arguments passed to methods.
#' @export
as_vrp_orders <- function(x, ...) {
  UseMethod("as_vrp_orders")
}

#' @export
as_vrp_orders.sfc <- function(x, ...) {
  check_stops(x)
  arcgisutils::as_esri_featureset(x)
}

#' @export
as_vrp_orders.sf <- function(x, verbose = TRUE, ...) {
  check_stops(sf::st_geometry(x))

  lu <- c(
    "name" = "Name",
    "description" = "Description",
    "service_time" = "ServiceTime",
    "time_window_start1" = "TimeWindowStart1",
    "time_window_end1" = "TimeWindowEnd1",
    "time_window_start2" = "TimeWindowStart2",
    "time_window_end2" = "TimeWindowEnd2",
    "max_violation_time1" = "MaxViolationTime1",
    "max_violation_time2" = "MaxViolationTime2",
    "inbound_arrive_time" = "InboundArriveTime",
    "outbound_depart_time" = "OutboundDepartTime",
    "delivery_quantities" = "DeliveryQuantities",
    "pickup_quantities" = "PickupQuantities",
    "revenue" = "Revenue",
    "specialty_names" = "SpecialtyNames",
    "assignment_rule" = "AssignmentRule",
    "curb_approach" = "CurbApproach",
    "route_name" = "RouteName",
    "sequence" = "Sequence",
    "bearing" = "Bearing",
    "bearing_tol" = "BearingTol",
    "nav_latency" = "NavLatency"
  )

  common_lu_vals <- na.omit(lu[colnames(x)])
  common_cols <- names(common_lu_vals)
  n_common <- length(common_lu_vals)

  if (verbose && n_common > 0) {
    cli::cli_alert_info("Using the provided attributes: {.col {common_cols}}")
  }

  if (n_common == 0) {
    return(as_vrp_orders(sf::st_geometry(x)))
  }

  n_col <- table(common_lu_vals)
  are_dupes <- n_col > 1
  if (any(are_dupes)) {
    cli::cli_abort(
      "Found duplicate column{?s}: {.col {names(n_col)[are_dupes]}}"
    )
  }

  datetime_cols <- c(
    "TimeWindowStart1",
    "TimeWindowEnd1",
    "TimeWindowStart2",
    "TimeWindowEnd2",
    "InboundArriveTime",
    "OutboundDepartTime"
  )
  string_cols <- c(
    "Name",
    "Description",
    "DeliveryQuantities",
    "PickupQuantities",
    "SpecialtyNames",
    "RouteName"
  )
  integer_cols <- c("AssignmentRule", "CurbApproach", "Sequence")
  numeric_cols <- c(
    "ServiceTime",
    "MaxViolationTime1",
    "MaxViolationTime2",
    "Revenue",
    "Bearing",
    "BearingTol",
    "NavLatency"
  )

  for (col in common_cols) {
    target <- lu[[col]]

    if (target %in% datetime_cols) {
      if (!arcgisutils::is_date(x[[col]])) {
        cli::cli_abort("{.col {col}} must be a date or datetime vector")
      }
      x[[col]] <- arcgisutils::date_to_ms(x[[col]])
    }

    if (target %in% string_cols) {
      check_character(x[[col]], arg = col)
    }

    if (target %in% integer_cols) {
      if (!is.numeric(x[[col]])) {
        cli::cli_abort("{.col {col}} must be a numeric vector")
      }
      x[[col]] <- as.integer(x[[col]])
    }

    if (target %in% numeric_cols) {
      if (!is.numeric(x[[col]])) {
        cli::cli_abort("{.col {col}} must be a numeric vector")
      }
    }

    if (target == "CurbApproach") {
      valid <- c(0L, 1L, 2L, 3L)
      invalid <- !x[[col]] %in% c(valid, NA_integer_)
      if (any(invalid)) {
        cli::cli_abort(
          "{.col {col}} must contain only values {.val {valid}} or NA"
        )
      }
    }

    if (target == "AssignmentRule") {
      valid <- 0L:5L
      invalid <- !x[[col]] %in% c(valid, NA_integer_)
      if (any(invalid)) {
        cli::cli_abort(
          "{.col {col}} must contain only values {.val {valid}} or NA"
        )
      }
    }

    if (target == "BearingTol") {
      b_range <- range(x[[col]], na.rm = TRUE)
      if (b_range[1] < 0 || b_range[2] > 180) {
        cli::cli_abort("{.col {col}} must be between 0 and 180 inclusive")
      }
    }
  }

  x <- x[common_cols]
  colnames(x) <- c(unname(common_lu_vals), "geometry")
  sf::st_geometry(x) <- "geometry"

  arcgisutils::as_esri_featureset(x)
}
