#' Find Closest Facility
#'
#' Finds one or more nearby facilities from incidents based on travel time or distance.
#'
#' @param incidents an `sf` or `sfc` object containing point geometries representing the locations to search from.
#' @param facilities an `sf` or `sfc` object containing point geometries representing the facilities to search for.
#' @param default_target_facility_count default `NULL`. An integer scalar. The number of closest facilities to find per incident.
#' @param default_cutoff default `NULL`. A numeric scalar. The travel time or distance value at which to stop searching for facilities.
#' @param time_of_day_usage default `NULL`. A scalar character. One of `"start_time"` or `"end_time"`. Specifies whether `time_of_day` represents departure or arrival time.
#' @param attribute_parameter_values default `NULL`. A list of objects. Additional values required by an attribute or restriction.
#' @param return_geometry default `c("facilities", "incidents")`. A character vector. Valid values: `"cf_routes"`, `"facilities"`, `"incidents"`, `"directions"`, `"barriers"`, `"polyline_barriers"`, `"polygon_barriers"`, `"traversed_edges"`, `"traversed_junctions"`, `"traversed_turns"`. Use `"everything"` to return all.
#' @param directions_output_type default `NULL`. A scalar character. One of `"standard"`, `"complete"`, `"complete_no_events"`, `"instructions_only"`, `"summary_only"`, `"feature_sets"`.
#' @param directions_style default `NULL`. A scalar character. One of `"desktop"`, `"navigation"`, `"campus"`.
#' @param directions_length_units default `NULL`. A scalar character. One of `"miles"`, `"kilometers"`, `"feet"`, `"meters"`, `"yards"`, `"nautical_miles"`.
#' @param directions_time_attribute default `NULL`. A scalar character. The time-based impedance attribute used for direction durations.
#' @param preserve_object_id default `FALSE`. A logical scalar. Preserves object IDs from input locations in the output.
#' @param return_empty_results default `FALSE`. A logical scalar. Returns empty results instead of an error on failure.
#' @param geometry_precision default `NULL`. An integer scalar. Decimal places for x and y values in response geometries.
#' @param geometry_precision_m default `NULL`. A scalar character. Decimal places for m-values in response geometries.
#' @param locate_settings default `NULL`. A list controlling how inputs are located on the network.
#' @inheritParams find_service_areas
#' @inheritParams find_routes
#'
#' @returns A named list. Elements present depend on `return_geometry`:
#' - `cf_routes`: route features between incidents and facilities
#' - `facilities`: facility features
#' - `incidents`: incident features
#' - `direction_points`: point features for direction maneuvers
#' - `direction_lines`: line features for route segments
#' - `barriers`: point barrier features
#' - `polyline_barriers`: polyline barrier features
#' - `polygon_barriers`: polygon barrier features
#' - `traversed_edges`: traversed edge features
#' - `traversed_junctions`: traversed junction features
#' - `traversed_turns`: traversed turn features
#' - `messages`: status and warning messages from the service
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(arcgisutils)
#' set_arc_token(auth_user())
#'
#' incidents <- st_sfc(st_point(c(-122.4496, 37.7467)), crs = 4326)
#'
#' facilities <- st_sf(
#'   name = c("Station 11", "Station 20", "Station 24", "Station 39"),
#'   geometry = st_sfc(
#'     st_point(c(-122.4267, 37.7486)),
#'     st_point(c(-122.4561, 37.7513)),
#'     st_point(c(-122.4409, 37.7533)),
#'     st_point(c(-122.4578, 37.7407)),
#'     crs = 4326
#'   )
#' )
#'
#' result <- find_closest_facility(
#'   incidents = incidents,
#'   facilities = facilities,
#'   default_target_facility_count = 2,
#'   travel_direction = "away",
#'   default_cutoff = 3,
#'   return_geometry = "cf_routes",
#'   directions_length_units = "miles",
#'   crs = 3857
#' )
#'
#' result
#' }
#'
#' @family direct
#' @family closest facility
#' @export
#' @references [API Reference](https://developers.arcgis.com/rest/routing/closest-facility-service-direct/)
find_closest_facility <- function(
  incidents,
  facilities,
  travel_mode = NULL,
  default_target_facility_count = NULL,
  travel_direction = NULL,
  default_cutoff = NULL,
  time_of_day = NULL,
  time_of_day_usage = NULL,
  u_turns = NULL,
  use_hierarchy = NULL,
  impedance = NULL,
  accumulate_impedance = NULL,
  restrictions = NULL,
  attribute_parameter_values = NULL,
  point_barriers = NULL,
  line_barriers = NULL,
  polygon_barriers = NULL,
  return_geometry = c("facilities", "incidents"),
  directions_language = NULL,
  directions_output_type = NULL,
  directions_style = NULL,
  directions_length_units = NULL,
  directions_time_attribute = NULL,
  output_lines = "true_shape",
  ignore_invalid_locations = TRUE,
  preserve_object_id = FALSE,
  return_empty_results = FALSE,
  output_geometry_precision = 10,
  output_geometry_precision_units = "meters",
  geometry_precision = NULL,
  geometry_precision_m = NULL,
  locate_settings = NULL,
  crs = 4326,
  token = arcgisutils::arc_token()
) {
  obj_check_token(token)

  check_bool(ignore_invalid_locations)
  check_bool(preserve_object_id)
  check_bool(return_empty_results)
  check_bool(use_hierarchy, allow_null = TRUE)
  check_number_decimal(output_geometry_precision)
  check_number_decimal(default_cutoff, allow_null = TRUE)
  check_number_whole(default_target_facility_count, allow_null = TRUE)
  check_number_whole(geometry_precision, allow_null = TRUE)
  check_string(geometry_precision_m, allow_null = TRUE)
  check_string(directions_language, allow_null = TRUE)
  check_string(directions_time_attribute, allow_null = TRUE)

  incidents <- as_stops(incidents)
  facilities <- as_stops(facilities)

  time_of_day <- validate_time_of_day(time_of_day)
  time_of_day_is_utc <- if (!is.null(time_of_day)) {
    is_utc(time_of_day)
  } else {
    FALSE
  }

  travel_direction <- validate_travel_dir(travel_direction)
  time_of_day_usage <- validate_time_of_day_usage(time_of_day_usage)
  u_turns <- validate_u_turns(u_turns)
  impedance <- validate_impedance_value(impedance)
  accumulate_impedance <- validate_impedance_value(
    accumulate_impedance,
    multiple = TRUE
  )
  restrictions <- validate_restrictions(restrictions)
  travel_mode <- validate_travel_mode(travel_mode, token = token)
  output_lines <- validate_output_lines(output_lines)
  output_geometry_precision_units <- validate_distance_units(
    output_geometry_precision_units
  )
  directions_output_type <- validate_directions_output_type(
    directions_output_type
  )
  directions_style <- validate_directions_style_name(directions_style)
  directions_length_units <- validate_directions_length_units(
    directions_length_units
  )
  return_geometry <- validate_return_geometry_cf(return_geometry)

  point_barriers <- as_point_barriers(point_barriers)
  line_barriers <- as_polyline_barriers(line_barriers)
  polygon_barriers <- as_polygon_barriers(polygon_barriers)

  params <- compact(list(
    incidents = incidents,
    facilities = facilities,
    travelMode = travel_mode,
    defaultTargetFacilityCount = default_target_facility_count,
    travelDirection = travel_direction,
    defaultCutoff = default_cutoff,
    timeOfDay = time_of_day,
    timeOfDayIsUTC = time_of_day_is_utc,
    timeOfDayUsage = time_of_day_usage,
    restrictUTurns = u_turns,
    useHierarchy = use_hierarchy,
    impedanceAttributeName = impedance,
    accumulateAttributeNames = accumulate_impedance,
    restrictionAttributeNames = restrictions,
    attributeParameterValues = attribute_parameter_values,
    barriers = point_barriers,
    polylineBarriers = line_barriers,
    polygonBarriers = polygon_barriers,
    returnCFRoutes = return_geometry$returnCFRoutes,
    returnFacilities = return_geometry$returnFacilities,
    returnIncidents = return_geometry$returnIncidents,
    returnDirections = return_geometry$returnDirections,
    returnBarriers = return_geometry$returnBarriers,
    returnPolylineBarriers = return_geometry$returnPolylineBarriers,
    returnPolygonBarriers = return_geometry$returnPolygonBarriers,
    returnTraversedEdges = return_geometry$returnTraversedEdges,
    returnTraversedJunctions = return_geometry$returnTraversedJunctions,
    returnTraversedTurns = return_geometry$returnTraversedTurns,
    directionsLanguage = directions_language,
    directionsOutputType = directions_output_type,
    directionsStyleName = directions_style,
    directionsLengthUnits = directions_length_units,
    directionsTimeAttributeName = directions_time_attribute,
    outputLines = output_lines,
    ignoreInvalidLocations = ignore_invalid_locations,
    preserveObjectID = preserve_object_id,
    returnEmptyResults = return_empty_results,
    outputGeometryPrecision = output_geometry_precision,
    outputGeometryPrecisionUnits = output_geometry_precision_units,
    geometryPrecision = geometry_precision,
    geometryPrecisionM = geometry_precision_m,
    locateSettings = locate_settings,
    outSR = arcgisutils::as_spatial_reference(crs),
    f = "json"
  ))

  meta <- detect_errors(arcgisutils::arc_portal_self(token))
  service_url <- meta$helperServices$closestFacility$url

  if (is.null(service_url)) {
    cli::cli_abort("Cannot find closest facility URL for this token")
  }

  resp <- arcgisutils::arc_base_req(
    service_url,
    token,
    path = "solveClosestFacility",
    query = c("f" = "json")
  ) |>
    httr2::req_body_form(!!!params, .multi = c("comma")) |>
    httr2::req_perform() |>
    httr2::resp_body_string()

  if (grepl("err", substr(resp, 1, 5))) {
    yyjsonr::read_json_str(resp) |>
      detect_errors()
  }

  compact(list(
    cf_routes = try_parse(resp, query = "/routes"),
    facilities = try_parse(resp, query = "/facilities"),
    incidents = try_parse(resp, query = "/incidents"),
    direction_points = try_parse(resp, query = "/directionPoints"),
    direction_lines = try_parse(resp, query = "/directionLines"),
    barriers = try_parse(resp, query = "/barriers"),
    polyline_barriers = try_parse(resp, query = "/polylineBarriers"),
    polygon_barriers = try_parse(resp, query = "/polygonBarriers"),
    traversed_edges = try_parse(resp, query = "/traversedEdges"),
    traversed_junctions = try_parse(resp, query = "/traversedJunctions"),
    traversed_turns = try_parse(resp, query = "/traversedTurns"),
    messages = RcppSimdJson::fparse(resp, query = "/messages")
  ))
}

validate_return_geometry_cf <- function(
  x,
  error_arg = rlang::caller_arg(x),
  error_call = rlang::caller_env()
) {
  check_character(x, arg = error_arg, call = error_call)

  all_values <- c(
    "cf_routes",
    "facilities",
    "incidents",
    "directions",
    "barriers",
    "polyline_barriers",
    "polygon_barriers",
    "traversed_edges",
    "traversed_junctions",
    "traversed_turns"
  )

  x <- if (identical(x, "everything")) {
    all_values
  } else {
    rlang::arg_match(
      x,
      values = all_values,
      multiple = TRUE,
      error_arg = error_arg,
      error_call = error_call
    )
  }

  return_geometry_lu <- c(
    "cf_routes" = "returnCFRoutes",
    "facilities" = "returnFacilities",
    "incidents" = "returnIncidents",
    "directions" = "returnDirections",
    "barriers" = "returnBarriers",
    "polyline_barriers" = "returnPolylineBarriers",
    "polygon_barriers" = "returnPolygonBarriers",
    "traversed_edges" = "returnTraversedEdges",
    "traversed_junctions" = "returnTraversedJunctions",
    "traversed_turns" = "returnTraversedTurns"
  )

  api_params <- return_geometry_lu[x]
  as.list(
    setNames(rep(TRUE, length(api_params)), api_params)
  )
}

validate_time_of_day_usage <- function(
  x,
  error_arg = rlang::caller_arg(x),
  error_call = rlang::caller_call()
) {
  if (is.null(x)) {
    return(NULL)
  }

  check_string(x, arg = error_arg, call = error_call)

  x <- rlang::arg_match0(
    x,
    values = c("start_time", "end_time"),
    arg_nm = error_arg,
    error_call = error_call
  )

  lu <- c(
    "start_time" = "esriNATimeOfDayUseAsStartTime",
    "end_time" = "esriNATimeOfDayUseAsEndTime"
  )

  unname(lu[x])
}

validate_directions_length_units <- function(
  x,
  error_arg = rlang::caller_arg(x),
  error_call = rlang::caller_call()
) {
  if (is.null(x)) {
    return(NULL)
  }

  check_string(x, arg = error_arg, call = error_call)

  x <- rlang::arg_match0(
    x,
    values = c(
      "miles",
      "kilometers",
      "feet",
      "meters",
      "yards",
      "nautical_miles"
    ),
    arg_nm = error_arg,
    error_call = error_call
  )

  lu <- c(
    "miles" = "esriNAUMiles",
    "kilometers" = "esriNAUKilometers",
    "feet" = "esriNAUFeet",
    "meters" = "esriNAUMeters",
    "yards" = "esriNAUYards",
    "nautical_miles" = "esriNAUNauticalMiles"
  )

  unname(lu[x])
}
