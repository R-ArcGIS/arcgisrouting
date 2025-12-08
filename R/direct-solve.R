# find_routes() function scaffold
#
# Based on ArcGIS /solve API endpoint documentation
#
# REQUIRED PARAMETERS:
# - stops: point features (use as_stops())
# - token: authentication token (default: arcgisutils::arc_token())
#
# OPTIONAL PARAMETERS WITH EXISTING VALIDATION:
#
# Travel Mode & Network:
# - travel_mode: validate_travel_mode()
# - impedance_attribute_name: validate_impedance_value()
# - accumulate_attribute_names: validate_impedance_value(multiple = TRUE)
# - restrict_u_turns: validate_u_turns()
# - restrictions: validate_restrictions()
# - use_hierarchy: check_bool()
#
# Timing:
# - start_time: arcgisutils::date_to_ms() (always UTC)
# - use_time_windows: DERIVED from stops having TimeWindowStart/TimeWindowEnd
#
# Route Optimization:
# - find_best_sequence: check_bool() (default: FALSE)
# - preserve_first_stop: check_bool() (default: TRUE, only applies if find_best_sequence = TRUE)
# - preserve_last_stop: check_bool() (default: TRUE, only applies if find_best_sequence = TRUE)
#
# Barriers:
# - barriers: as_point_barriers()
# - polyline_barriers: as_polyline_barriers()
# - polygon_barriers: as_polygon_barriers()
#
# Directions:
# - return_directions: check_bool() (default: TRUE)
# - directions_language: check_string() (default: "en")
# - directions_output_type: NEEDS validate_directions_output_type()
# - directions_style_name: NEEDS validate_directions_style()
# - directions_length_units: NEEDS validate_length_units()
# - directions_time_attribute_name: validate_impedance_value() (subset to time-based)
#
# Output Control:
# - return_routes: check_bool() (default: TRUE)
# - return_stops: check_bool() (default: FALSE)
# - return_barriers: check_bool() (default: FALSE)
# - return_polyline_barriers: check_bool() (default: FALSE)
# - return_polygon_barriers: check_bool() (default: FALSE)
# - output_lines: NEEDS validate_output_lines()
#
# Advanced/Debug:
# - return_traversed_edges: check_bool() (default: FALSE)
# - return_traversed_junctions: check_bool() (default: FALSE)
# - return_traversed_turns: check_bool() (default: FALSE)
#
# Error Handling:
# - ignore_invalid_locations: check_bool() (default: TRUE)
# - return_empty_results: check_bool() (default: FALSE)
#
# Geometry:
# - output_geometry_precision: check_number_decimal()
# - output_geometry_precision_units: NEEDS validate_precision_units()
# - geometry_precision: check_number_whole()
# - geometry_precision_m: check_number_whole()
#
# Object ID:
# - preserve_object_id: check_bool() (default: FALSE)
#
# PARAMETERS TO SKIP/HANDLE DIFFERENTLY:
# - attributeParameterValues: Complex nested structure - handle later if needed
# - context: Internal parameter for spatial reference - handle via arcgisutils
# - overrides: Internal use only
#
# VALIDATION FUNCTIONS NEEDED:
# - validate_directions_output_type()
# - validate_directions_style()
# - validate_length_units()
# - validate_output_lines()
# - validate_precision_units()
#
# NOTES:
# - Remove all UTC-related booleans (date_to_ms() always uses UTC)
# - Many parameters override travel_mode settings
# - use_time_windows should be automatically derived from stops data

#' Find Routes
#'
#' Finds the best routes between multiple stops using the ArcGIS routing service.
#'
#' @param stops An `sf` or `sfc` object containing point geometries representing
#'   the stops to visit. Use [as_stops()] to prepare stops with attributes.
#' @param travel_mode Character. The name of the travel mode to use. See
#'   [get_travel_modes()] for available options. Default: `NULL`.
#' @param start_time POSIXct or character. The time at which travel begins.
#'   Can be `"now"` for current time, a POSIXct datetime, or `NULL` for
#'   static speeds. Default: `NULL`.
#' @param find_best_sequence Logical. Whether to reorder stops to find the
#'   optimized route. Default: `FALSE`.
#' @param preserve_first_stop Logical. Whether to keep the first stop fixed
#'   when reordering. Only applies if `find_best_sequence = TRUE`. Default: `TRUE`.
#' @param preserve_last_stop Logical. Whether to keep the last stop fixed
#'   when reordering. Only applies if `find_best_sequence = TRUE`. Default: `TRUE`.
#' @param restrict_u_turns Character. Specifies U-turn restrictions. One of:
#'   `"allow_backtrack"`, `"deadend_intersection"`, `"deadend"`, `"no_backtrack"`.
#'   Default: `"allow_backtrack"`.
#' @param use_hierarchy Logical. Whether to use hierarchy when finding routes.
#'   Default: `TRUE`.
#' @param impedance_attribute_name Character. The impedance to use. One of:
#'   `"travel_time"`, `"minutes"`, `"truck_travel_time"`, `"truck_minutes"`,
#'   `"walk_time"`, `"miles"`, `"kilometers"`. Default: `NULL`.
#' @param accumulate_attribute_names Character vector. Additional impedance
#'   values to accumulate. Default: `NULL`.
#' @param restrictions Character vector. Restriction names to honor.
#'   Default: `NULL`.
#' @param barriers Point barriers as `sf` or `sfc` object. Default: `NULL`.
#' @param polyline_barriers Line barriers as `sf` or `sfc` object. Default: `NULL`.
#' @param polygon_barriers Polygon barriers as `sf` or `sfc` object. Default: `NULL`.
#' @param return_directions Logical. Whether to generate driving directions.
#'   Default: `TRUE`.
#' @param directions_language Character. Language code for directions (e.g., `"en"`).
#'   Default: `"en"`.
#' @param return_routes Logical. Whether to return route geometries. Default: `TRUE`.
#' @param return_stops Logical. Whether to return stops in output. Default: `FALSE`.
#' @param ignore_invalid_locations Logical. Whether to ignore invalid locations.
#'   Default: `TRUE`.
#' @param token Authorization token. Default: [arcgisutils::arc_token()].
#'
#' @returns A list containing the routing results.
#'
#' @export
find_routes <- function(
  stops,
  travel_mode = NULL,
  start_time = NULL,
  find_best_sequence = FALSE,
  preserve_first_stop = TRUE,
  preserve_last_stop = TRUE,
  restrict_u_turns = NULL,
  use_hierarchy = NULL,
  impedance_attribute_name = NULL,
  accumulate_attribute_names = NULL,
  restrictions = NULL,
  barriers = NULL,
  polyline_barriers = NULL,
  polygon_barriers = NULL,
  return_directions = TRUE,
  directions_language = "en",
  return_routes = TRUE,
  return_stops = FALSE,
  ignore_invalid_locations = TRUE,
  token = arcgisutils::arc_token()
) {
  obj_check_token(token)
  stops <- as_stops(stops)

  check_bool(return_stops)
  check_bool(return_routes)
  check_bool(return_directions)
  check_bool(preserve_last_stop)
  check_bool(find_best_sequence)
  check_bool(preserve_first_stop)
  check_string(directions_language)
  check_bool(ignore_invalid_locations)
  check_bool(use_hierarchy, allow_null = TRUE)

  start_time <- validate_start_time(start_time)
  restrictions <- validate_restrictions(restrictions)
  restrict_u_turns <- validate_u_turns(restrict_u_turns)
  travel_mode <- validate_travel_mode(travel_mode, token = token)
  impedance_attribute_name <- validate_impedance_value(impedance_attribute_name)

  accumulate_attribute_names <- validate_impedance_value(
    accumulate_attribute_names,
    multiple = TRUE
  )

  # barrier handling
  barriers <- as_point_barriers(barriers)
  polyline_barriers <- as_polyline_barriers(polyline_barriers)
  polygon_barriers <- as_polygon_barriers(polygon_barriers)

  # we set this value from the input stops
  use_time_windows <- detect_time_windows(stops)

  # Build parameters list
  params <- compact(list(
    stops = stops,
    travelMode = travel_mode,
    startTime = start_time,
    findBestSequence = find_best_sequence,
    preserveFirstStop = preserve_first_stop,
    preserveLastStop = preserve_last_stop,
    useTimeWindows = use_time_windows,
    restrictUTurns = restrict_u_turns,
    useHierarchy = use_hierarchy,
    impedanceAttributeName = impedance_attribute_name,
    accumulateAttributeNames = accumulate_attribute_names,
    restrictionAttributeNames = restrictions,
    barriers = barriers,
    polylineBarriers = polyline_barriers,
    polygonBarriers = polygon_barriers,
    returnDirections = return_directions,
    directionsLanguage = directions_language,
    returnRoutes = return_routes,
    returnStops = return_stops,
    ignoreInvalidLocations = ignore_invalid_locations,
    f = "json"
  ))

  # Get service URL
  meta <- arcgisutils::arc_portal_self(token)
  service_url <- meta$helperServices$route$url

  if (is.null(service_url)) {
    cli::cli_abort("Cannot find routing service URL for this token")
  }

  resp <- arcgisutils::arc_base_req(
    service_url,
    token,
    path = "solve"
  ) |>
    httr2::req_body_form(!!!params) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    yyjsonr::read_json_str()

  # Check for errors
  arcgisutils::detect_errors(resp)

  resp
}


validate_start_time <- function(x, error_call = rlang::caller_env()) {
  if (is.null(x)) {
    return(NULL)
  }

  if (is.character(x) && tolower(x) == "now") {
    return("now")
  }

  if (arcgisutils::is_date(x)) {
    return(arcgisutils::date_to_ms(x))
  }

  cli::cli_abort(
    "{.arg start_time} must be coercable to a date vector",
    call = error_call
  )
}

detect_time_windows <- function(stops) {
  if (inherits(stops, "sfc")) {
    return(FALSE)
  }

  if (!inherits(stops, "sf")) {
    return(FALSE)
  }

  time_window_cols <- c(
    "time_window_start",
    "time_window_end",
    "TimeWindowStart",
    "TimeWindowEnd"
  )

  any(time_window_cols %in% colnames(stops))
}
