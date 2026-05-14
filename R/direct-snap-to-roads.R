snap_to_roads <- function(
  points,
  travel_mode = NULL,
  point_properties = c(
    "oid_routing_streets",
    "posted_speed_limit_kph",
    "posted_speed_limit_mph",
    "posted_speed_limit_mps",
    "posted_truck_speed_limit_kph",
    "posted_truck_speed_limit_mph",
    "posted_truck_speed_limit_mps"
  ),
  line_properties = c(
    "oid_routing_streets",
    "length_kilometers",
    "length_miles",
    "posted_speed_limit_kph",
    "posted_speed_limit_mph",
    "posted_speed_limit_mps",
    "posted_truck_speed_limit_kph",
    "posted_truck_speed_limit_mph",
    "posted_truck_speed_limit_mps"
  ),
  analysis_region = c(
    "europe",
    "japan",
    "korea",
    "middleastandafrica",
    "northamerica",
    "southamerica",
    "southasia",
    "thailand"
  ),
  return_lines = TRUE,
  token = arc_token()
) {
  obj_check_token(token)
  check_bool(return_lines)

  # browser()
  points <- as_snap_points(points)
  travel_mode <- validate_travel_mode(travel_mode, token = token)
  analysis_region <- validate_snap_analysis_region(analysis_region)

  point_properties <- validate_point_properties(point_properties)
  line_properties <- validate_line_properties(line_properties)

  params <- compact(list(
    points = points,
    travel_mode = travel_mode,
    return_lines = return_lines,
    road_properties_on_snapped_points = yyjsonr::write_json_str(
      point_properties
    ),
    road_properties_on_lines = yyjsonr::write_json_str(line_properties),
    analysis_region = analysis_region,
    f = "json"
  ))

  meta <- detect_errors(arcgisutils::arc_portal_self(token))
  service_url <- meta$helperServices$snapToRoads$url

  if (is.null(service_url)) {
    cli::cli_abort("Cannot find routing service URL for this token")
  }

  resp <- arcgisutils::arc_base_req(
    service_url,
    token,
    path = c("SnapToRoads", "execute"),
    query = c("f" = "json")
  ) |>
    httr2::req_body_form(!!!params, .multi = c("comma")) |>
    httr2::req_perform() |>
    httr2::resp_body_string()

  if (grepl("err", substr(resp, 1, 5))) {
    yyjsonr::read_json_str(resp) |>
      detect_errors()
  }

  cost_idx <- if (return_lines) 2L else 1L

  compact(list(
    snapped_points = try_parse(resp, query = "/results/0/value"),
    snap_lines = if (return_lines) try_parse(resp, query = "/results/1/value"),
    usage_cost = rlang::try_fetch(
      RcppSimdJson::fparse(
        resp,
        query = sprintf("/results/%d/value", cost_idx)
      ),
      error = function(e) NULL
    ),
    messages = RcppSimdJson::fparse(resp, query = "/messages")
  ))
}

validate_snap_analysis_region <- function(
  x,
  error_arg = rlang::caller_arg(x),
  error_call = rlang::caller_call()
) {
  if (is.null(x)) {
    return(NULL)
  }

  x <- rlang::arg_match0(
    x,
    values = c(
      "europe",
      "japan",
      "korea",
      "middleastandafrica",
      "northamerica",
      "southamerica",
      "southasia",
      "thailand"
    ),
    arg_nm = error_arg,
    error_call = error_call
  )

  lu <- c(
    "europe" = "Europe",
    "japan" = "Japan",
    "korea" = "Korea",
    "middleastandafrica" = "MiddleEastAndAfrica",
    "northamerica" = "NorthAmerica",
    "southamerica" = "SouthAmerica",
    "southasia" = "SouthAsia",
    "thailand" = "Thailand"
  )

  unname(lu[x])
}

#' Convert spatial objects to snap points for the SnapToRoads service
#'
#' Prepares point geometries and their optional tracking attributes for use
#' with [snap_to_roads()]. An `sfc` input is accepted for geometry-only
#' snapping. An `sf` input allows specifying additional per-point attributes.
#'
#' Recognized attribute columns (matched case-insensitively):
#' - `object_id`, `objectid`, `oid`: integer, links input to output points
#' - `location_timestamp`: POSIXct, used to sequence points
#' - `horizontal_accuracy`: non-negative numeric, meters (equivalent to HDOP)
#' - `speed`: non-negative numeric, meters per second
#' - `course`: non-negative numeric, compass degrees 0-360
#' - `track_id`: character, groups points into separate tracks
#'
#' @param x An `sfc_POINT` or `sf` object with point geometries. Maximum 5,000 points.
#' @param ... Additional arguments passed to methods.
#' @keywords internal
#' @noRd
as_snap_points <- function(x, ...) {
  UseMethod("as_snap_points")
}

#' @export
as_snap_points.sfc <- function(x, ...) {
  check_snap_points(x)
  arcgisutils::as_esri_featureset(x)
}

#' @export
as_snap_points.sf <- function(x, ...) {
  check_snap_points(sf::st_geometry(x))

  snap_colname_lu <- c(
    "object_id" = "OBJECTID",
    "objectid" = "OBJECTID",
    "oid" = "OBJECTID",
    "location_timestamp" = "location_timestamp",
    "horizontal_accuracy" = "horizontal_accuracy",
    "speed" = "speed",
    "course" = "course",
    "track_id" = "track_id"
  )

  lower_cols <- tolower(colnames(x))
  matched <- lower_cols %in% names(snap_colname_lu)
  common_cols <- colnames(x)[matched]
  common_lower <- lower_cols[matched]

  if (length(common_cols) == 0L) {
    return(as_snap_points(sf::st_geometry(x)))
  }

  api_names <- snap_colname_lu[common_lower]

  n_col <- table(api_names)
  are_dupes <- n_col > 1

  if (any(are_dupes)) {
    dupe_api_cols <- names(n_col)[are_dupes]
    cli::cli_abort(
      "Found duplicate column{?s} mapping to: {.col {dupe_api_cols}}"
    )
  }

  for (i in seq_along(common_cols)) {
    col <- common_cols[[i]]
    target_col <- api_names[[i]]

    if (target_col == "OBJECTID") {
      if (!is.numeric(x[[col]])) {
        cli::cli_abort("{.col {col}} must be a numeric vector")
      }
      x[[col]] <- as.integer(x[[col]])
    }

    if (target_col == "location_timestamp") {
      if (!arcgisutils::is_date(x[[col]])) {
        cli::cli_abort("{.col {col}} must be a date vector")
      }
      x[[col]] <- arcgisutils::date_to_ms(x[[col]])
    }

    if (target_col %in% c("horizontal_accuracy", "speed", "course")) {
      if (!is.numeric(x[[col]])) {
        cli::cli_abort("{.col {col}} must be a numeric vector")
      }
      if (any(x[[col]] < 0, na.rm = TRUE)) {
        cli::cli_abort("{.col {col}} must be non-negative")
      }
    }

    if (target_col == "track_id") {
      check_character(x[[col]], arg = col)
    }
  }

  x <- x[common_cols]
  colnames(x) <- c(api_names, "geometry")
  sf::st_geometry(x) <- "geometry"

  arcgisutils::as_esri_featureset(x)
}

validate_point_properties <- function(
  x,
  error_arg = rlang::caller_arg(x),
  error_call = rlang::caller_call()
) {
  if (is.null(x)) {
    return(NULL)
  }

  check_character(x, allow_null = TRUE, arg = error_arg, call = error_call)

  if (length(x) == 0L) {
    cli::cli_abort(
      "{.arg {error_arg}} must not be empty",
      call = error_call
    )
  }

  rlang::arg_match(
    x,
    values = c(
      "oid_routing_streets",
      "posted_speed_limit_kph",
      "posted_speed_limit_mph",
      "posted_speed_limit_mps",
      "posted_truck_speed_limit_kph",
      "posted_truck_speed_limit_mph",
      "posted_truck_speed_limit_mps"
    ),
    multiple = TRUE,
    error_arg = error_arg,
    error_call = error_call
  )
}

validate_line_properties <- function(
  x,
  error_arg = rlang::caller_arg(x),
  error_call = rlang::caller_call()
) {
  if (is.null(x)) {
    return(NULL)
  }

  check_character(x, allow_null = TRUE, arg = error_arg, call = error_call)

  if (length(x) == 0L) {
    cli::cli_abort(
      "{.arg {error_arg}} must not be empty",
      call = error_call
    )
  }

  rlang::arg_match(
    x,
    values = c(
      "oid_routing_streets",
      "length_kilometers",
      "length_miles",
      "posted_speed_limit_kph",
      "posted_speed_limit_mph",
      "posted_speed_limit_mps",
      "posted_truck_speed_limit_kph",
      "posted_truck_speed_limit_mph",
      "posted_truck_speed_limit_mps"
    ),
    multiple = TRUE,
    error_arg = error_arg,
    error_call = error_call
  )
}

check_snap_points <- function(x, error_call = rlang::caller_env()) {
  if (!inherits(x, "sfc_POINT")) {
    cli::cli_abort(
      "Snap points must be point geometries not {obj_type_friendly(x)}",
      call = error_call
    )
  }

  if (is.na(sf::st_crs(x))) {
    cli::cli_abort(
      c("!" = "`crs` is not set. Please set the crs."),
      call = error_call
    )
  }

  n <- vctrs::vec_size(x)
  if (n > 5000L) {
    cli::cli_abort(
      c(
        "The maximum number of snap points is {.val {5000L}}",
        "i" = "Found {.val {n}} points"
      ),
      call = error_call
    )
  }
}
