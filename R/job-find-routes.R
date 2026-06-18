.find_routes_job <- R6::R6Class(
  "find_routes_job",
  inherit = arcgisutils::arc_gp_job
)

#' Find Routes (Async)
#'
#' Submits an asynchronous geoprocessing job to find the best routes between
#' stops using the ArcGIS `/FindRoutes` GP service.
#'
#' @param stops An `sf` or `sfc` object containing point geometries.
#'   Recognized attribute columns such as `route_name`, `time_window_start`,
#'   and `time_window_end` are used when present.
#' @param travel_mode Character. The name or ID of the travel mode to use.
#'   See [get_travel_modes()] for available options. Default: `NULL`.
#' @param measurement_units Character. Units for reporting total travel time or
#'   distance. One of: `"meters"`, `"kilometers"`, `"feet"`, `"yards"`,
#'   `"miles"`, `"nautical_miles"`, `"seconds"`, `"minutes"`, `"hours"`,
#'   `"days"`. Default: `NULL` (API default: `"minutes"`).
#' @param analysis_region Character. Region for the analysis. One of:
#'   `"europe"`, `"japan"`, `"korea"`, `"middle_east_and_africa"`,
#'   `"north_america"`, `"south_america"`, `"south_asia"`, `"thailand"`.
#'   Default: `NULL` (auto-detected).
#' @param reorder_stops Logical. Reorder stops to find the optimal route
#'   (TSP). Default: `FALSE`.
#' @param preserve_terminal_stops Character. Which terminal stops to preserve
#'   when `reorder_stops = TRUE`. One of: `"first"`, `"last"`,
#'   `"first_and_last"`, `"none"`. Default: `NULL` (API default: `"first"`).
#' @param return_to_start Logical. Whether the route should return to its
#'   starting location. Default: `NULL` (API default: `TRUE`).
#' @param use_time_windows Logical. Whether to honour time windows on stops.
#'   Default: `NULL` (auto-detected from `stops` attributes).
#' @param time_of_day POSIXct. The departure time for the routes. When `NULL`,
#'   static average speeds are used. Default: `NULL`.
#' @param uturn_at_junctions Character. U-turn policy at junctions. One of:
#'   `"allow_backtrack"`, `"deadend_intersection"`, `"deadend"`,
#'   `"no_backtrack"`. Default: `NULL`.
#' @param use_hierarchy Logical. Whether to use the street hierarchy when
#'   finding routes. Default: `NULL` (API default: `TRUE`).
#' @param restrictions Character vector. Restriction names to apply.
#'   Default: `NULL`.
#' @param impedance Character. Impedance type. One of: `"travel_time"`,
#'   `"minutes"`, `"truck_travel_time"`, `"truck_minutes"`, `"walk_time"`,
#'   `"miles"`, `"kilometers"`. Default: `NULL`.
#' @param time_impedance Character. Time-based impedance. One of:
#'   `"minutes"`, `"travel_time"`, `"walk_time"`, `"truck_minutes"`,
#'   `"truck_travel_time"`. Default: `NULL`.
#' @param distance_impedance Character. Distance-based impedance. One of:
#'   `"miles"`, `"kilometers"`. Default: `NULL`.
#' @param route_shape Character. Shape of the output route features. One of:
#'   `"true_shape"`, `"true_shape_with_measures"`, `"straight_line"`,
#'   `"none"`. Default: `NULL` (API default: `"true_shape"`).
#' @param populate_route_edges Logical. Generate edges for each route.
#'   Default: `NULL` (API default: `FALSE`).
#' @param populate_directions Logical. Generate driving directions.
#'   Default: `NULL` (API default: `FALSE`).
#' @param directions_language Character. Language code for directions.
#'   Default: `NULL` (API default: `"en"`).
#' @param directions_distance_units Character. Units for distances in
#'   directions. One of: `"feet"`, `"kilometers"`, `"meters"`, `"miles"`,
#'   `"nautical_miles"`, `"yards"`. Default: `NULL`.
#' @param directions_style_name Character. Formatting style for directions.
#'   One of: `"desktop"`, `"navigation"`. Default: `NULL`.
#' @param output_format Character. Format for output features. One of:
#'   `"feature_set"`, `"json_file"`, `"geojson_file"`. Default:
#'   `"feature_set"`.
#' @param ignore_invalid_locations Logical. Whether to ignore invalid
#'   input locations. Default: `TRUE`.
#' @param point_barriers Point barriers as `sf` or `sfc` object.
#'   Default: `NULL`.
#' @param line_barriers Line barriers as `sf` or `sfc` object.
#'   Default: `NULL`.
#' @param polygon_barriers Polygon barriers as `sf` or `sfc` object.
#'   Default: `NULL`.
#' @param token Authorization token. Default: [arcgisutils::arc_token()].
#'
#' @returns A `find_routes_job` R6 object inheriting from
#'   `arcgisutils::arc_gp_job`. Call `$start()` to submit and `$results` to
#'   retrieve output.
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(arcgisutils)
#' set_arc_token(auth_user())
#'
#' stops <- st_sf(
#'   name = c("Stop 1", "Stop 2", "Stop 3"),
#'   geometry = st_sfc(
#'     st_point(c(145.066, -37.865)),
#'     st_point(c(145.105, -37.819)),
#'     st_point(c(145.120, -37.800)),
#'     crs = 4326
#'   )
#' )
#'
#' job <- find_routes_job(stops)
#' job$start()
#' result <- job$results
#' }
#'
#' @family async
#' @family routing
#' @export
#' @references [API Reference](https://developers.arcgis.com/rest/routing/find-routes/)
find_routes_job <- function(
  stops,
  travel_mode = NULL,
  measurement_units = NULL,
  analysis_region = NULL,
  reorder_stops = FALSE,
  preserve_terminal_stops = NULL,
  return_to_start = NULL,
  use_time_windows = NULL,
  time_of_day = NULL,
  uturn_at_junctions = NULL,
  use_hierarchy = NULL,
  restrictions = NULL,
  impedance = NULL,
  time_impedance = NULL,
  distance_impedance = NULL,
  route_shape = NULL,
  populate_route_edges = NULL,
  populate_directions = NULL,
  directions_language = NULL,
  directions_distance_units = NULL,
  directions_style_name = NULL,
  output_format = "feature_set",
  ignore_invalid_locations = TRUE,
  point_barriers = NULL,
  line_barriers = NULL,
  polygon_barriers = NULL,
  token = arcgisutils::arc_token()
) {
  obj_check_token(token)
  stops <- as_stops(stops)

  check_bool(reorder_stops)
  check_bool(return_to_start, allow_null = TRUE)
  check_bool(use_hierarchy, allow_null = TRUE)
  check_bool(populate_route_edges, allow_null = TRUE)
  check_bool(populate_directions, allow_null = TRUE)
  check_bool(ignore_invalid_locations)
  check_string(directions_language, allow_null = TRUE)

  travel_mode <- validate_travel_mode(travel_mode, token = token)
  measurement_units <- validate_measurement_units(measurement_units)
  analysis_region <- validate_analysis_region(analysis_region)
  preserve_terminal_stops <- validate_preserve_terminal_stops(
    preserve_terminal_stops
  )
  time_of_day <- validate_time_of_day(time_of_day)
  time_zone_for_time_of_day <- validate_tz_for_time_of_day(time_of_day)
  uturn_at_junctions <- validate_u_turns_async(uturn_at_junctions)
  restrictions <- validate_restrictions(restrictions)
  impedance <- validate_impedance_value(impedance)
  time_impedance <- validate_time_impedance(time_impedance)
  distance_impedance <- validate_distance_impedance(distance_impedance)
  route_shape <- validate_route_shape(route_shape)
  directions_distance_units <- validate_directions_distance_units(
    directions_distance_units
  )
  directions_style_name <- validate_directions_style_name(directions_style_name)
  output_format <- validate_job_output_format(output_format)

  if (is.null(use_time_windows)) {
    use_time_windows <- detect_time_windows(stops)
  }

  point_barriers <- as_point_barriers(point_barriers)
  line_barriers <- as_polyline_barriers(line_barriers)
  polygon_barriers <- as_polygon_barriers(polygon_barriers)

  params <- compact(list(
    stops = stops,
    travel_mode = travel_mode,
    measurement_units = measurement_units,
    analysis_region = analysis_region,
    reorder_stops_to_find_optimal_routes = reorder_stops,
    preserve_terminal_stops = preserve_terminal_stops,
    return_to_start = return_to_start,
    use_time_windows = use_time_windows,
    time_of_day = time_of_day,
    time_zone_for_time_of_day = time_zone_for_time_of_day,
    uturn_at_junctions = uturn_at_junctions,
    use_hierarchy = use_hierarchy,
    restrictions = restrictions,
    impedance = impedance,
    time_impedance = time_impedance,
    distance_impedance = distance_impedance,
    route_shape = route_shape,
    populate_route_edges = populate_route_edges,
    populate_directions = populate_directions,
    directions_language = directions_language,
    directions_distance_units = directions_distance_units,
    directions_style_name = directions_style_name,
    output_format = output_format,
    ignore_invalid_locations = ignore_invalid_locations,
    point_barriers = point_barriers,
    line_barriers = line_barriers,
    polygon_barriers = polygon_barriers,
    f = "json"
  ))

  meta <- arcgisutils::arc_portal_self(token)
  base_url <- meta$helperServices$asyncRoute$url

  if (is.null(base_url)) {
    cli::cli_abort("Cannot find async route service URL for this token")
  }

  job_url <- httr2::req_url_path_append(
    httr2::request(base_url),
    "FindRoutes"
  )$url

  .find_routes_job$new(
    job_url,
    arcgisutils::as_form_params(params)@params,
    # \(res) res,
    parse_find_routes_results,
    token = token
  )
}

parse_find_routes_results <- function(json) {
  compact(list(
    solve_succeeded = RcppSimdJson::fparse(json, query = "/0/value"),
    routes = try_parse(json, "/1/value"),
    route_edges = try_parse(json, "/2/value"),
    directions = try_parse(json, "/3/value"),
    stops = try_parse(json, "/4/value"),
    output_network_analysis_layer = RcppSimdJson::fparse(
      json,
      query = "/5/value"
    ),
    output_route_data = RcppSimdJson::fparse(json, query = "/6/value"),
    output_result_file = RcppSimdJson::fparse(json, query = "/7/value"),
    output_network_analysis_layer_package = RcppSimdJson::fparse(
      json,
      query = "/8/value"
    ),
    direction_points = try_parse(json, "/9/value"),
    direction_lines = try_parse(json, "/10/value"),
    usage_cost = RcppSimdJson::fparse(json, query = "/11/value")
  ))
}


validate_measurement_units <- function(
  x,
  error_arg = rlang::caller_arg(x),
  error_call = rlang::caller_call()
) {
  if (is.null(x)) {
    return(NULL)
  }

  lu <- c(
    "meters" = "Meters",
    "kilometers" = "Kilometers",
    "feet" = "Feet",
    "yards" = "Yards",
    "miles" = "Miles",
    "nautical_miles" = "NauticalMiles",
    "seconds" = "Seconds",
    "minutes" = "Minutes",
    "hours" = "Hours",
    "days" = "Days"
  )

  x <- rlang::arg_match(
    x,
    names(lu),
    error_arg = error_arg,
    error_call = error_call
  )
  unname(lu[x])
}


validate_preserve_terminal_stops <- function(
  x,
  error_arg = rlang::caller_arg(x),
  error_call = rlang::caller_call()
) {
  if (is.null(x)) {
    return(NULL)
  }

  lu <- c(
    "first" = "Preserve First",
    "last" = "Preserve Last",
    "first_and_last" = "Preserve First and Last",
    "none" = "Preserve None"
  )

  x <- rlang::arg_match(
    x,
    names(lu),
    error_arg = error_arg,
    error_call = error_call
  )
  unname(lu[x])
}


validate_route_shape <- function(
  x,
  error_arg = rlang::caller_arg(x),
  error_call = rlang::caller_call()
) {
  if (is.null(x)) {
    return(NULL)
  }

  lu <- c(
    "true_shape" = "True Shape",
    "true_shape_with_measures" = "True Shape with Measures",
    "straight_line" = "Straight Line",
    "none" = "None"
  )

  x <- rlang::arg_match(
    x,
    names(lu),
    error_arg = error_arg,
    error_call = error_call
  )
  unname(lu[x])
}


validate_directions_distance_units <- function(
  x,
  error_arg = rlang::caller_arg(x),
  error_call = rlang::caller_call()
) {
  if (is.null(x)) {
    return(NULL)
  }

  lu <- c(
    "feet" = "Feet",
    "kilometers" = "Kilometers",
    "meters" = "Meters",
    "miles" = "Miles",
    "nautical_miles" = "NauticalMiles",
    "yards" = "Yards"
  )

  x <- rlang::arg_match(
    x,
    names(lu),
    error_arg = error_arg,
    error_call = error_call
  )
  unname(lu[x])
}


validate_job_output_format <- function(
  x,
  error_arg = rlang::caller_arg(x),
  error_call = rlang::caller_call()
) {
  if (is.null(x)) {
    return(NULL)
  }

  lu <- c(
    "feature_set" = "Feature Set",
    "json_file" = "JSON File",
    "geojson_file" = "GeoJSON File"
  )

  x <- rlang::arg_match(
    x,
    names(lu),
    error_arg = error_arg,
    error_call = error_call
  )
  unname(lu[x])
}
