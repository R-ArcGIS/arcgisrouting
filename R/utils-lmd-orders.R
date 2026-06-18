# Builds a snake_case -> API lookup for a numbered attribute family such as
# DeliveryQuantity_1..9 or Capacity_1..9. Returns a named character vector.
numbered_field_lu <- function(snake_prefix, api_prefix, n = 9L) {
  idx <- seq_len(n)
  stats::setNames(
    paste0(api_prefix, "_", idx),
    paste0(snake_prefix, "_", idx)
  )
}

#' @noRd
#' @export
as_lmd_orders <- function(x, ...) {
  UseMethod("as_lmd_orders")
}

#' @export
as_lmd_orders.sfc <- function(x, ...) {
  check_stops(x)
  arcgisutils::as_esri_featureset(x)
}

#' @export
as_lmd_orders.sf <- function(x, verbose = TRUE, ...) {
  check_stops(sf::st_geometry(x))

  lu <- c(
    "name" = "Name",
    "service_time" = "ServiceTime",
    "time_window_start" = "TimeWindowStart",
    "time_window_end" = "TimeWindowEnd",
    "max_violation_time" = "MaxViolationTime",
    "inbound_arrive_time" = "InboundArriveTime",
    "outbound_depart_time" = "OutboundDepartTime",
    numbered_field_lu("delivery_quantity", "DeliveryQuantity"),
    numbered_field_lu("pickup_quantity", "PickupQuantity"),
    "anchor_rule" = "AnchorRule",
    "assignment_rule" = "AssignmentRule",
    "route_name" = "RouteName",
    "sequence" = "Sequence",
    "curb_approach" = "CurbApproach",
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
    return(as_lmd_orders(sf::st_geometry(x)))
  }

  n_col <- table(common_lu_vals)
  are_dupes <- n_col > 1
  if (any(are_dupes)) {
    cli::cli_abort(
      "Found duplicate column{?s}: {.col {names(n_col)[are_dupes]}}"
    )
  }

  datetime_cols <- c(
    "TimeWindowStart",
    "TimeWindowEnd",
    "InboundArriveTime",
    "OutboundDepartTime"
  )
  string_cols <- c("Name", "RouteName")
  integer_cols <- c(
    "AnchorRule",
    "AssignmentRule",
    "Sequence",
    "CurbApproach",
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
      if (!rlang::is_integerish(x[[col]])) {
        cli::cli_abort("{.col {col}} must be a whole number vector")
      }
      x[[col]] <- as.integer(x[[col]])
    }

    # Numbered quantities and ServiceTime/MaxViolationTime are plain doubles
    if (
      startsWith(target, "DeliveryQuantity_") ||
        startsWith(target, "PickupQuantity_") ||
        target %in% c("ServiceTime", "MaxViolationTime")
    ) {
      if (!is.numeric(x[[col]])) {
        cli::cli_abort("{.col {col}} must be a numeric vector")
      }
    }

    if (target == "CurbApproach") {
      rng <- range(x[[col]], na.rm = TRUE)
      if (rng[1] < 0 || rng[2] > 3) {
        cli::cli_abort("{.col {col}} must be between 0 and 3 inclusive")
      }
    }

    if (target == "AssignmentRule") {
      rng <- range(x[[col]], na.rm = TRUE)
      if (rng[1] < 1 || rng[2] > 3) {
        cli::cli_abort("{.col {col}} must be between 1 and 3 inclusive")
      }
    }

    if (target == "AnchorRule") {
      rng <- range(x[[col]], na.rm = TRUE)
      if (rng[1] < 1 || rng[2] > 2) {
        cli::cli_abort("{.col {col}} must be between 1 and 2 inclusive")
      }
    }

    if (target == "BearingTol") {
      rng <- range(x[[col]], na.rm = TRUE)
      if (rng[1] < 0 || rng[2] > 180) {
        cli::cli_abort("{.col {col}} must be between 0 and 180 inclusive")
      }
    }
  }

  x <- x[common_cols]
  colnames(x) <- c(unname(common_lu_vals), "geometry")
  sf::st_geometry(x) <- "geometry"

  arcgisutils::as_esri_featureset(x)
}
