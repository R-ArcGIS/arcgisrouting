# Serializes a date vector to the "YYYY-MM-DD" strings the API expects. Used
# for the EarliestStartDate route attribute and the earliest_route_start_date
# top-level default.
validate_start_date <- function(
  x,
  error_arg = rlang::caller_arg(x),
  error_call = rlang::caller_call()
) {
  if (is.null(x)) {
    return(NULL)
  }
  if (!arcgisutils::is_date(x)) {
    cli::cli_abort(
      "{.arg {error_arg}} must be a date vector",
      call = error_call
    )
  }
  format(x)
}

# Serializes a time-of-day vector to the "hh:mm:ss" strings the API expects.
# Used for the EarliestStartTime route attribute and the
# earliest_route_start_time top-level default.
validate_start_time <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  as.character(hms::parse_hms(x))
}

#' Convert a data.frame to Last Mile Delivery routes input
#'
#' @param x A `data.frame` describing vehicle and driver characteristics,
#'   or `NULL`.
#' @param ... Additional arguments passed to methods.
#' @export
as_lmd_routes <- function(x, ...) {
  UseMethod("as_lmd_routes")
}

#' @export
as_lmd_routes.NULL <- function(x, ...) {
  NULL
}

#' @export
as_lmd_routes.data.frame <- function(x, ...) {
  lu <- c(
    "name" = "Name",
    "start_depot_name" = "StartDepotName",
    "end_depot_name" = "EndDepotName",
    "start_depot_service_time" = "StartDepotServiceTime",
    "end_depot_service_time" = "EndDepotServiceTime",
    "earliest_start_date" = "EarliestStartDate",
    "earliest_start_time" = "EarliestStartTime",
    "start_flexibility" = "StartFlexibility",
    "arrive_depart_delay" = "ArriveDepartDelay",
    numbered_field_lu("capacity", "Capacity"),
    "fixed_cost" = "FixedCost",
    "cost_per_unit_time" = "CostPerUnitTime",
    "cost_per_unit_distance" = "CostPerUnitDistance",
    "overtime_start_time" = "OvertimeStartTime",
    "cost_per_unit_overtime" = "CostPerUnitOverTime",
    "max_order_count" = "MaxOrderCount",
    "max_total_time" = "MaxTotalTime",
    "max_total_travel_time" = "MaxTotalTravelTime",
    "max_total_distance" = "MaxTotalDistance",
    "zone_name" = "ZoneName",
    "is_hard_zone" = "IsHardZone",
    "assignment_rule" = "AssignmentRule"
  )

  common_cols <- intersect(names(lu), colnames(x))

  if (length(common_cols) == 0L) {
    cli::cli_abort(
      c(
        "No recognized columns found in {.arg routes}",
        "i" = "Recognized columns: {.val {names(lu)}}"
      )
    )
  }

  common_lu_vals <- lu[common_cols]

  n_col <- table(common_lu_vals)
  are_dupes <- n_col > 1
  if (any(are_dupes)) {
    cli::cli_abort("Found duplicate column{?s}: {names(n_col)[are_dupes]}")
  }

  string_cols <- c("Name", "StartDepotName", "EndDepotName", "ZoneName")
  # EarliestStartDate is a date string; EarliestStartTime is a time-only field
  non_numeric_cols <- c(string_cols, "EarliestStartDate", "EarliestStartTime")

  for (col in common_cols) {
    target <- lu[[col]]

    if (target %in% string_cols) {
      check_character(x[[col]], arg = col)
    }

    if (target == "EarliestStartDate") {
      x[[col]] <- validate_start_date(x[[col]], error_arg = col)
    }

    if (target == "EarliestStartTime") {
      x[[col]] <- validate_start_time(x[[col]])
    }

    if (target == "IsHardZone") {
      check_logical(x[[col]], arg = col)
      x[[col]] <- as.integer(x[[col]])
    }

    if (target %in% c("MaxOrderCount", "AssignmentRule")) {
      if (!rlang::is_integerish(x[[col]])) {
        cli::cli_abort("{.col {col}} must be a whole number vector")
      }
      x[[col]] <- as.integer(x[[col]])
    }

    if (target == "AssignmentRule") {
      rng <- range(x[[col]], na.rm = TRUE)
      if (rng[1] < 1 || rng[2] > 2) {
        cli::cli_abort("{.col {col}} must be between 1 and 2 inclusive")
      }
    }

    # All remaining recognized fields are non-negative doubles
    if (
      !(target %in% c(non_numeric_cols, "IsHardZone")) &&
        !(target %in% c("MaxOrderCount", "AssignmentRule"))
    ) {
      if (!is.numeric(x[[col]])) {
        cli::cli_abort("{.col {col}} must be a numeric vector")
      }
    }
  }

  x <- x[common_cols]
  colnames(x) <- unname(common_lu_vals)

  arcgisutils::as_esri_featureset(x)
}
