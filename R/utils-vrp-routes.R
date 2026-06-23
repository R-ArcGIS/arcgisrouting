#' Convert a data.frame to VRP routes input
#'
#' @param x A `data.frame` describing vehicle and driver characteristics,
#'   or `NULL`.
#' @param ... Additional arguments passed to methods.
#' @keywords internal
#' @noRd
as_routes <- function(x, ...) {
  UseMethod("as_routes")
}

#' @export
as_routes.NULL <- function(x, ...) {
  NULL
}

#' @export
as_routes.data.frame <- function(x, ...) {
  routes_colname_lu <- c(
    "name" = "Name",
    "start_depot_name" = "StartDepotName",
    "end_depot_name" = "EndDepotName",
    "start_depot_service_time" = "StartDepotServiceTime",
    "end_depot_service_time" = "EndDepotServiceTime",
    "earliest_start_time" = "EarliestStartTime",
    "latest_start_time" = "LatestStartTime",
    "arrive_depart_delay" = "ArriveDepartDelay",
    "capacities" = "Capacities",
    "fixed_cost" = "FixedCost",
    "cost_per_unit_time" = "CostPerUnitTime",
    "cost_per_unit_distance" = "CostPerUnitDistance",
    "overtime_start_time" = "OvertimeStartTime",
    "cost_per_unit_overtime" = "CostPerUnitOvertime",
    "max_order_count" = "MaxOrderCount",
    "max_total_time" = "MaxTotalTime",
    "max_total_travel_time" = "MaxTotalTravelTime",
    "max_total_distance" = "MaxTotalDistance",
    "specialty_names" = "SpecialtyNames",
    "assignment_rule" = "AssignmentRule"
  )

  common_cols <- intersect(names(routes_colname_lu), colnames(x))

  if (length(common_cols) == 0L) {
    cli::cli_abort(
      c(
        "No recognized columns found in {.arg routes}",
        "i" = "Recognized columns: {.val {names(routes_colname_lu)}}"
      )
    )
  }

  common_lu_vals <- routes_colname_lu[common_cols]

  n_col <- table(common_lu_vals)
  are_dupes <- n_col > 1
  if (any(are_dupes)) {
    dupe_cols <- names(n_col)[are_dupes]
    cli::cli_abort("Found duplicate column{?s}: {dupe_cols}")
  }

  for (col in common_cols) {
    target_col <- routes_colname_lu[[col]]

    if (
      target_col %in%
        c(
          "Name",
          "StartDepotName",
          "EndDepotName",
          "SpecialtyNames",
          "Capacities"
        )
    ) {
      check_character(x[[col]], arg = col)
    }

    if (target_col %in% c("EarliestStartTime", "LatestStartTime")) {
      if (!arcgisutils::is_date(x[[col]])) {
        cli::cli_abort("{.col {col}} must be a date or datetime vector")
      }
      x[[col]] <- arcgisutils::date_to_ms(x[[col]])
    }

    if (target_col == "AssignmentRule") {
      if (!is.numeric(x[[col]])) {
        cli::cli_abort("{.col {col}} must be a numeric vector")
      }
      x[[col]] <- as.integer(x[[col]])
      invalid <- !x[[col]] %in% c(0L, 1L, NA_integer_)
      if (any(invalid)) {
        cli::cli_abort(
          "{.col {col}} must contain only values {.val {c(0L, 1L)}} or NA"
        )
      }
    }

    if (target_col == "MaxOrderCount") {
      if (!is.numeric(x[[col]])) {
        cli::cli_abort("{.col {col}} must be a numeric vector")
      }
      x[[col]] <- as.integer(x[[col]])
    }

    if (
      target_col %in%
        c(
          "StartDepotServiceTime",
          "EndDepotServiceTime",
          "ArriveDepartDelay",
          "FixedCost",
          "CostPerUnitTime",
          "CostPerUnitDistance",
          "OvertimeStartTime",
          "CostPerUnitOvertime",
          "MaxTotalTime",
          "MaxTotalTravelTime",
          "MaxTotalDistance"
        )
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
