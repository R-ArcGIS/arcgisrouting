# Stops attributes documentation from ArcGIS REST API
# Name: string (length: 500) nullable
# ObjectID: integer (non-negative) nullable
# RouteName: string (length: 1024) nullable
# Sequence: integer nullable
# TimeWindowStart: datetime nullable (milliseconds since epoch)
# TimeWindowEnd: datetime nullable (milliseconds since epoch)
# CurbApproach: int enum default:0 (allowed: 0, 1, 2, 3)
# Attr_[Cost]: number (non-negative) default:0 nullable
# LocationType: int enum default:0 (allowed: 0, 1, 2)
#   0: Stop—A location that the route will visit
#   1: Waypoint—A location that the route will travel through without making a stop
#   2: Break—A location where the route stops for the driver to take a break
# Bearing: number (non-negative) nullable
# BearingTol: number (range: 0 - 180) default:30 nullable
# NavLatency: number (non-negative) nullable

#' Convert spatial objects to stops for routing
#'
#' @param x An `sfc` or `sf` object containing point geometries
#' @param ... Additional arguments passed to methods
#' @export
as_stops <- function(x, ...) {
  UseMethod("as_stops")
}

#' @export
as_stops.sfc <- function(x, ...) {
  check_stops(x)
  arcgisutils::as_esri_featureset(x)
}

# Validation function for stops
check_stops <- function(x, error_call = rlang::caller_env()) {
  if (!inherits(x, "sfc_POINT")) {
    cli::cli_abort(
      "Stops must be point geometries not {obj_type_friendly(x)}",
      call = error_call
    )
  }

  if (is.na(sf::st_crs(x))) {
    cli::cli_abort(c("!" = "`crs` is not set. Please set the crs."))
  }

  n <- vctrs::vec_size(x)

  if (n > 10000L) {
    cli::cli_abort(
      c(
        "The maximum number of stops supported is {.val {10000L}}",
        "i" = "Found {.val {n}} stops",
        "i" = "If this is a limitation for you, please report it",
        " " = "{.url https://github.com/r-arcgis/arcgisrouting/issues}"
      ),
      call = error_call
    )
  }
}

#' @export
as_stops.sf <- function(x, verbose = TRUE, ...) {
  check_stops(sf::st_geometry(x))

  stop_colname_lu <- c(
    "name" = "Name",
    "object_id" = "ObjectID",
    "objectID" = "ObjectID",
    "objectid" = "ObjectID",
    "ObjectID" = "ObjectID",
    "route_name" = "RouteName",
    "sequence" = "Sequence",
    "time_window_start" = "TimeWindowStart",
    "time_window_end" = "TimeWindowEnd",
    "curb_approach" = "CurbApproach",
    "location_type" = "LocationType",
    "bearing" = "Bearing",
    "bearing_tol" = "BearingTol",
    "nav_latency" = "NavLatency"
  )

  # Find valid columns
  common_cols <- intersect(
    names(stop_colname_lu),
    colnames(x)
  )

  common_lu_vals <- na.omit(stop_colname_lu[colnames(x)])
  n_common <- length(common_lu_vals)

  # Inform user about which attributes are being used
  if (verbose && n_common > 0) {
    cli::cli_alert_info("Using the provided attributes: {.col {common_cols}}")
  }

  # If no recognized columns, just convert geometry
  if (n_common == 0) {
    return(as_stops(sf::st_geometry(x)))
  }

  # Check for duplicate columns mapping to same API field
  n_col <- table(common_lu_vals)
  are_dupes <- n_col > 1
  dupe_cols <- names(n_col)[are_dupes]

  if (any(are_dupes)) {
    cli::cli_abort("Found duplicate column{?s}: {dupe_cols}")
  }

  # Validate individual columns
  for (col in common_cols) {
    target_col <- stop_colname_lu[[col]]

    # Name validation
    if (target_col == "Name") {
      check_character(x[[col]], arg = col)
    }

    if (target_col == "RouteName") {
      # RouteName validation
      check_character(x[[col]], arg = col)
    }

    if (target_col == "Sequence") {
      # Sequence validation
      if (!is.numeric(x[[col]])) {
        cli::cli_abort("{.col {col}} must be a numeric vector")
      }
      x[[col]] <- as.integer(x[[col]])
    }

    if (target_col %in% c("TimeWindowStart", "TimeWindowEnd")) {
      if (!arcgisutils::is_date(x[[col]])) {
        cli::cli_abort("{.col {col}} must be a date vector")
      }
    }

    if (target_col == "CurbApproach") {
      # CurbApproach validation
      if (!is.numeric(x[[col]])) {
        cli::cli_abort("{.col {col}} must be a numeric vector")
      }
      x[[col]] <- as.integer(x[[col]])

      valid_values <- c(0L, 1L, 2L, 3L)
      invalid <- !x[[col]] %in% c(valid_values, NA_integer_)
      if (any(invalid)) {
        cli::cli_abort(
          "{.col {col}} must contain only values {.val {valid_values}} or NA"
        )
      }
    }

    if (target_col == "LocationType") {
      # LocationType validation
      if (!is.numeric(x[[col]])) {
        cli::cli_abort("{.col {col}} must be a numeric vector")
      }
      x[[col]] <- as.integer(x[[col]])

      valid_values <- c(0L, 1L, 2L)
      invalid <- !x[[col]] %in% c(valid_values, NA_integer_)
      if (any(invalid)) {
        cli::cli_abort(
          "{.col {col}} must contain only values {.val {valid_values}} or NA",
          "i" = "0 = Stop, 1 = Waypoint, 2 = Break"
        )
      }
    }

    if (target_col == "Bearing") {
      # Bearing validation
      if (!is.numeric(x[[col]])) {
        cli::cli_abort("{.col {col}} must be a numeric vector")
      }
    }

    if (target_col == "BearingTol") {
      # BearingTol validation
      if (!is.numeric(x[[col]])) {
        cli::cli_abort("{.col {col}} must be a numeric vector")
      }

      b_range <- range(x[[col]], na.rm = TRUE)

      if (b_range[1] < 0 || b_range[2] > 180) {
        cli::cli_abort("{.col {col}} must be between 0 and 180 inclusive")
      }
    }

    if (target_col == "NavLatency") {
      # NavLatency validation
      if (!is.numeric(x[[col]])) {
        cli::cli_abort("{.col {col}} must be a numeric vector")
      }
    }

    if (startsWith(target_col, "Attr_")) {
      # Impedance attribute validation (Attr_*)
      if (!rlang::is_bare_numeric(x[[col]])) {
        cli::cli_abort(
          "Expected impedance column {.col {col}} to be numeric. Found {obj_type_friendly(x[[col]])}."
        )
      }
    }
  }

  # Validate NavLatency requires Bearing and BearingTol
  if (
    "NavLatency" %in%
      common_lu_vals &&
      !("Bearing" %in% common_lu_vals && "BearingTol" %in% common_lu_vals)
  ) {
    cli::cli_abort(
      "{.col nav_latency} provided but not accompanied by {.col bearing} and {.col bearing_tol}"
    )
  }

  # Subset and reorder columns
  x <- x[common_cols]
  colnames(x) <- c(na.omit(common_lu_vals), "geometry")
  sf::st_geometry(x) <- "geometry"

  arcgisutils::as_esri_featureset(x)
}
