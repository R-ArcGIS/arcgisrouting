# VALIDATION FUNCTIONS NEEDED:
# - validate_directions_output_type()
# - validate_directions_style()
# - validate_length_units()
# - validate_output_lines()
# - validate_precision_units()

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
#' @param directions_type Character. Specifies the content and verbosity of driving
#'   directions (`directionsOutputType` in REST API). One of: `"complete"`,
#'   `"complete_no_events"`, `"instructions_only"`, `"standard"`, `"summary_only"`,
#'   `"feature_sets"`. Default: `"standard"`.
#' @param return_geometry Character vector. Specifies which features to return
#'   in the output. Valid values: `"routes"`, `"directions"`, `"stops"`,
#'   `"barriers"`, `"polyline_barriers"`, `"polygon_barriers"`,
#'   `"traversed_edges"`, `"traversed_junctions"`, `"traversed_turns"`.
#'   Default: `c("routes", "directions")`.
#' @param ignore_invalid_locations Logical. Whether to ignore invalid locations.
#'   Default: `TRUE`.
#' @param token Authorization token. Default: [arcgisutils::arc_token()].
#'
#' @returns A list containing the routing resps. The elements returned depend
#'   on the `return_geometry` parameter. Possible elements include:
#'   - `routes`: Route features
#'   - `directions`: Driving directions
#'   - `stops`: Stop features
#'   - `barriers`: Barrier features
#'   - `polyline_barriers`: Polyline barrier features
#'   - `polygon_barriers`: Polygon barrier features
#'   - `traversed_edges`: Traversed edge features
#'   - `traversed_junctions`: Traversed junction features
#'   - `traversed_turns`: Traversed turn features
#'   - `messages`: Status and warning messages from the service
#'
#'   When `directions_type = "feature_sets"`, the response includes:
#'   - `direction_points`: sf object with point features for direction maneuvers
#'   - `direction_lines`: sf object with line features for route segments
#'
#' @examples
#' \dontrun{
#' library(sf)
#'
#' # Simple route between 3 stops
#' stops <- st_sf(
#'   name = c("Start", "Middle", "End"),
#'   geometry = st_sfc(
#'     st_point(c(-122.4194, 37.7749)),  # San Francisco
#'     st_point(c(-122.0312, 37.3318)),  # Cupertino
#'     st_point(c(-121.8863, 37.3382)),  # San Jose
#'     crs = 4326
#'   )
#' )
#'
#' resp <- find_routes(stops)
#'
#' # Route with time windows for deliveries
#' delivery_stops <- st_sf(
#'   name = c("Warehouse", "Customer A", "Customer B", "Customer C"),
#'   time_window_start = as.POSIXct(c(
#'     "2024-01-15 08:00:00",
#'     "2024-01-15 09:00:00",
#'     "2024-01-15 11:00:00",
#'     "2024-01-15 14:00:00"
#'   )),
#'   time_window_end = as.POSIXct(c(
#'     "2024-01-15 08:30:00",
#'     "2024-01-15 10:00:00",
#'     "2024-01-15 13:00:00",
#'     "2024-01-15 16:00:00"
#'   )),
#'   geometry = st_sfc(
#'     st_point(c(-122.4194, 37.7749)),
#'     st_point(c(-122.4083, 37.7858)),
#'     st_point(c(-122.4313, 37.7793)),
#'     st_point(c(-122.4000, 37.7900)),
#'     crs = 4326
#'   )
#' )
#'
#' delivery_route <- find_routes(
#'   delivery_stops,
#'   start_time = as.POSIXct("2024-01-15 08:00:00"),
#'   travel_mode = "Driving Time"
#' )
#'
#' # Find optimal order for stops
#' sales_stops <- st_sf(
#'   name = c("Office", "Client 1", "Client 2", "Client 3", "Office"),
#'   geometry = st_sfc(
#'     st_point(c(-122.4194, 37.7749)),
#'     st_point(c(-122.4083, 37.7858)),
#'     st_point(c(-122.4313, 37.7793)),
#'     st_point(c(-122.4000, 37.7900)),
#'     st_point(c(-122.4194, 37.7749)),
#'     crs = 4326
#'   )
#' )
#'
#' optimized_route <- find_routes(
#'   sales_stops,
#'   find_best_sequence = TRUE,
#'   preserve_first_stop = TRUE,
#'   preserve_last_stop = TRUE
#' )
#'
#' # Route with barriers
#' barrier_pts <- st_sfc(
#'   st_point(c(-122.4150, 37.7800)),
#'   crs = 4326
#' )
#'
#' route_with_barriers <- find_routes(
#'   stops,
#'   barriers = barrier_pts
#' )
#'
#' # Accumulate multiple impedances
#' multi_impedance_route <- find_routes(
#'   stops,
#'   impedance_attribute_name = "travel_time",
#'   accumulate_attribute_names = c("miles", "kilometers")
#' )
#' }
#'
#' @export
#' @references [API Reference](https://developers.arcgis.com/rest/routing/route-service-direct/)
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
  directions_language = "en",
  directions_type = "standard",
  return_geometry = c("routes", "directions"),
  ignore_invalid_locations = TRUE,
  token = arcgisutils::arc_token()
) {
  obj_check_token(token)
  stops <- as_stops(stops)

  check_bool(preserve_last_stop)
  check_bool(find_best_sequence)
  check_bool(preserve_first_stop)
  check_string(directions_language)
  check_bool(ignore_invalid_locations)
  check_bool(use_hierarchy, allow_null = TRUE)

  return_geometry <- validate_return_geometry(return_geometry)

  start_time <- validate_start_time(start_time)
  restrictions <- validate_restrictions(restrictions)
  restrict_u_turns <- validate_u_turns(restrict_u_turns)
  travel_mode <- validate_travel_mode(travel_mode, token = token)
  impedance_attribute_name <- validate_impedance_value(impedance_attribute_name)
  directions_type <- validate_directions_output_type(directions_type)

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
    returnDirections = return_geometry$returnDirections,
    directionsLanguage = directions_language,
    directionsOutputType = directions_type,
    returnRoutes = return_geometry$returnRoutes,
    returnStops = return_geometry$returnStops,
    returnBarriers = return_geometry$returnBarriers,
    returnPolylineBarriers = return_geometry$returnPolylineBarriers,
    returnPolygonBarriers = return_geometry$returnPolygonBarriers,
    returnTraversedEdges = return_geometry$returnTraversedEdges,
    returnTraversedJunctions = return_geometry$returnTraversedJunctions,
    returnTraversedTurns = return_geometry$returnTraversedTurns,
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
    httr2::req_body_form(!!!params, .multi = c("comma")) |>
    httr2::req_perform() |>
    httr2::resp_body_string()

  compact(list(
    directions = .process_directions(resp),
    routes = try_parse(resp, query = "/routes"),
    stops = try_parse(resp, query = "/stops"),
    barriers = try_parse(resp, query = "/barriers"),
    traversed_junctions = try_parse(resp, query = "/traversedJunctions"),
    polyline_barriers = try_parse(resp, query = "/polylineBarriers"),
    polygon_barriers = try_parse(resp, query = "/polygonBarriers"),
    traversed_edges = try_parse(resp, query = "/traversedEdges"),
    traversed_turns = try_parse(resp, query = "/traversedTurns"),
    direction_points = try_parse(resp, query = "/directionPoints"),
    direction_lines = try_parse(resp, query = "/directionLines"),
    messages = RcppSimdJson::fparse(resp, query = "/messages"),
    checksum = RcppSimdJson::fparse(resp, query = "/checksum")
  ))
}

# process the summary
.process_summary <- function(x, error_call = rlang::caller_call()) {
  x$envelope <- arcgisutils::from_envelope(
    x$envelope,
    error_call = error_call
  )
}

# process the attributes of directions result
.process_attributes <- function(x) {
  lapply(x, \(.x) {
    vctrs::new_data_frame(.x)
  }) |>
    rbind_results() |>
    data_frame()
}

# an individual featureset
.process_feature <- function(x) {
  res <- .process_attributes(x$attributes)
  res$compress_geometry <- x$compressedGeometry
  res$strings <- x$strings
  res
}

# process all directions
.process_directions <- function(result) {
  x <- RcppSimdJson::fparse(result, query = "/directions")
  x$directions <- lapply(x$features, .process_feature)
  x$features <- NULL
  x$summary <- lapply(x$summary, .process_summary)
  data_frame(x)
}


# safely try and parse the respant geometries
try_parse <- function(resp, query) {
  rlang::try_fetch(
    parse_esri_json(resp, query = query),
    error = function(e) {
      if (grepl("^NO_SUCH_FIELD", e$message)) {
        NULL
      } else {
        cli::cli_alert_warning(e$message)
        invisible(NULL)
      }
    }
  )
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

validate_directions_output_type <- function(
  x,
  error_arg = rlang::caller_arg(x),
  error_call = rlang::caller_env()
) {
  if (is.null(x)) {
    return(NULL)
  }

  valid_values <- c(
    "complete",
    "complete_no_events",
    "instructions_only",
    "standard",
    "summary_only",
    "feature_sets"
  )

  x <- rlang::arg_match0(
    x,
    values = valid_values,
    arg_nm = error_arg,
    error_call = error_call
  )

  lu <- c(
    "complete" = "esriDOTComplete",
    "complete_no_events" = "esriDOTCompleteNoEvents",
    "instructions_only" = "esriDOTInstructionsOnly",
    "standard" = "esriDOTStandard",
    "summary_only" = "esriDOTSummaryOnly",
    "feature_sets" = "esriDOTFeatureSets"
  )

  unname(lu[x])
}

validate_directions_style_name <- function(
  x,
  error_arg = rlang::caller_arg(x),
  error_call = rlang::caller_env()
) {
  if (is.null(x)) {
    return(NULL)
  }

  valid_values <- c(
    "desktop",
    "navigation",
    "campus"
  )

  x <- rlang::arg_match0(
    x,
    values = valid_values,
    error_arg = error_arg,
    error_call = error_call
  )

  lu <- c(
    "desktop" = "NA Desktop",
    "navigation" = "NA Navigation",
    "campus" = "NA Campus"
  )

  unname(lu[x])
}

validate_return_geometry <- function(
  x,
  error_arg = rlang::caller_arg(x),
  error_call = rlang::caller_env()
) {
  check_character(x, arg = error_arg, call = error_call)

  return_geometry_lu <- c(
    "routes" = "returnRoutes",
    "directions" = "returnDirections",
    "stops" = "returnStops",
    "barriers" = "returnBarriers",
    "polyline_barriers" = "returnPolylineBarriers",
    "polygon_barriers" = "returnPolygonBarriers",
    "traversed_edges" = "returnTraversedEdges",
    "traversed_junctions" = "returnTraversedJunctions",
    "traversed_turns" = "returnTraversedTurns"
  )

  invalid <- setdiff(x, names(return_geometry_lu))

  if (length(invalid) > 0) {
    cli::cli_abort(
      c(
        "{.arg {error_arg}} contains invalid values: {.val {invalid}}",
        "i" = "Valid values are: {.val {names(return_geometry_lu)}}"
      ),
      call = error_call
    )
  }

  api_params <- return_geometry_lu[x]
  as.list(
    setNames(rep(TRUE, length(api_params)), api_params)
  )
}
