.od_cost_matrix_job <- R6::R6Class(
  "od_cost_matrix_job",
  inherit = arcgisutils::arc_gp_job
)

#' Generate Origin-Destination Cost Matrix (Async)
#'
#' Submits an asynchronous geoprocessing job that creates an origin-destination
#' (OD) cost matrix from multiple origins to multiple destinations using the
#' ArcGIS `/GenerateOriginDestinationCostMatrix` GP service. The matrix reports
#' the travel time or travel distance from every origin to every destination.
#'
#' @param origins An `sf` or `sfc` object containing point geometries that
#'   function as starting points in generating the paths to destinations.
#' @param destinations An `sf` or `sfc` object containing point geometries that
#'   function as ending points in generating the paths from origins. Default:
#'   `NULL`.
#' @param time_units Character. Units used to measure and report the total
#'   travel time between each origin-destination pair. One of: `"seconds"`,
#'   `"minutes"`, `"hours"`, `"days"`. Default: `NULL` (API default:
#'   `"minutes"`).
#' @param distance_units Character. Units used to measure and report the total
#'   travel distance between each origin-destination pair. One of: `"meters"`,
#'   `"kilometers"`, `"feet"`, `"yards"`, `"miles"`, `"nautical_miles"`.
#'   Default: `NULL` (API default: `"miles"`).
#' @param n_dests Integer. The maximum number of destinations to find per
#'   origin. Default: `NULL` (API default: every destination).
#' @param cutoff Numeric. The travel time or travel distance value at which to
#'   stop searching for destinations from a given origin. Default: `NULL`.
#' @param origin_destination_line_shape Character. Shape of the line feature
#'   connecting each origin-destination pair in the output matrix. One of:
#'   `"straight_line"` or `"none"`. Default: `NULL` (API default: `"none"`).
#' @inheritParams location_allocation_job
#'
#' @returns An `od_cost_matrix_job` R6 object inheriting from
#'   `arcgisutils::arc_gp_job`. Call `$start()` to submit and `$results` to
#'   retrieve output.
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(arcgisutils)
#' set_arc_token(auth_user())
#'
#' origins <- st_sf(
#'   name = c("Origin 1", "Origin 2"),
#'   n_dests = c(2L, 3L),
#'   cutoff = c(120, 90),
#'   curb_approach = c(0L, 0L),
#'   geometry = st_sfc(
#'     st_point(c(-0.1891, 51.5254)),
#'     st_point(c(-0.1744, 51.5353)),
#'     crs = 4326
#'   )
#' )
#'
#' destinations <- st_sf(
#'   name = c("Destination 1", "Destination 2"),
#'   curb_approach = c(0L, 0L),
#'   geometry = st_sfc(
#'     st_point(c(-0.1991, 51.5354)),
#'     st_point(c(-0.1844, 51.5458)),
#'     crs = 4326
#'   )
#' )
#'
#' job <- od_cost_matrix_job(origins, destinations)
#' job$start()
#' result <- job$results
#' }
#'
#' @family async
#' @family od
#' @export
#' @references [API Reference](https://developers.arcgis.com/rest/routing/travelCostMatrix-service-job/)
od_cost_matrix_job <- function(
  origins,
  destinations = NULL,
  travel_mode = NULL,
  time_units = NULL,
  distance_units = NULL,
  analysis_region = NULL,
  n_dests = NULL,
  cutoff = NULL,
  time_of_day = NULL,
  uturn_at_junctions = NULL,
  use_hierarchy = NULL,
  restrictions = NULL,
  attribute_parameter_values = NULL,
  origin_destination_line_shape = NULL,
  impedance = NULL,
  time_impedance = NULL,
  distance_impedance = NULL,
  point_barriers = NULL,
  line_barriers = NULL,
  polygon_barriers = NULL,
  save_output_network_analysis_layer = NULL,
  output_format = "feature_set",
  ignore_invalid_locations = TRUE,
  token = arcgisutils::arc_token()
) {
  obj_check_token(token)
  origins <- as_od_points(origins)
  destinations <- as_od_points(destinations)

  check_number_whole(n_dests, allow_null = TRUE, min = 0)
  check_number_decimal(cutoff, min = 0, allow_null = TRUE)
  check_bool(use_hierarchy, allow_null = TRUE)
  check_bool(save_output_network_analysis_layer, allow_null = TRUE)
  check_bool(ignore_invalid_locations)

  travel_mode <- validate_travel_mode(travel_mode, token = token)
  time_units <- validate_time_units(time_units)
  distance_units <- validate_distance_units(distance_units)
  analysis_region <- validate_analysis_region(analysis_region)
  time_of_day <- validate_time_of_day(time_of_day)
  time_zone_for_time_of_day <- validate_tz_for_time_of_day(time_of_day)
  uturn_at_junctions <- validate_u_turns_async(uturn_at_junctions)
  restrictions <- validate_restrictions(restrictions)
  origin_destination_line_shape <- validate_allocation_line_shape(
    origin_destination_line_shape
  )
  impedance <- validate_impedance_value(impedance)
  time_impedance <- validate_time_impedance(time_impedance)
  distance_impedance <- validate_distance_impedance(distance_impedance)
  output_format <- validate_job_output_format(output_format)

  point_barriers <- as_point_barriers(point_barriers)
  line_barriers <- as_polyline_barriers(line_barriers)
  polygon_barriers <- as_polygon_barriers(polygon_barriers)

  params <- compact(list(
    origins = origins,
    destinations = destinations,
    travel_mode = travel_mode,
    time_units = time_units,
    distance_units = distance_units,
    analysis_region = analysis_region,
    number_of_destinations_to_find = n_dests,
    cutoff = cutoff,
    time_of_day = time_of_day,
    time_zone_for_time_of_day = time_zone_for_time_of_day,
    uturn_at_junctions = uturn_at_junctions,
    use_hierarchy = use_hierarchy,
    restrictions = restrictions,
    attribute_parameter_values = attribute_parameter_values,
    origin_destination_line_shape = origin_destination_line_shape,
    impedance = impedance,
    time_impedance = time_impedance,
    distance_impedance = distance_impedance,
    point_barriers = point_barriers,
    line_barriers = line_barriers,
    polygon_barriers = polygon_barriers,
    save_output_network_analysis_layer = save_output_network_analysis_layer,
    output_format = output_format,
    ignore_invalid_locations = ignore_invalid_locations,
    f = "json"
  ))

  meta <- arcgisutils::arc_portal_self(token)
  base_url <- meta$helperServices$asyncODCostMatrix$url

  if (is.null(base_url)) {
    cli::cli_abort("Cannot find async OD cost matrix URL for this token")
  }

  job_url <- httr2::req_url_path_append(
    httr2::request(base_url),
    "GenerateOriginDestinationCostMatrix"
  )$url

  .od_cost_matrix_job$new(
    job_url,
    arcgisutils::as_form_params(params)@params,
    parse_od_cost_matrix_results,
    token = token
  )
}

parse_od_cost_matrix_results <- function(json) {
  compact(list(
    solve_succeeded = RcppSimdJson::fparse(json, query = "/0/value"),
    origin_destination_lines = try_parse(json, "/1/value"),
    origins = try_parse(json, "/2/value"),
    destinations = try_parse(json, "/3/value"),
    output_network_analysis_layer = RcppSimdJson::fparse(
      json,
      query = "/4/value"
    ),
    output_result_file = RcppSimdJson::fparse(json, query = "/5/value"),
    output_network_analysis_layer_package = RcppSimdJson::fparse(
      json,
      query = "/6/value"
    ),
    usage_cost = RcppSimdJson::fparse(json, query = "/7/value")
  ))
}

validate_distance_units <- function(
  distance_units,
  error_arg = rlang::caller_arg(distance_units),
  error_call = rlang::caller_call()
) {
  if (is.null(distance_units)) {
    return(NULL)
  }
  lu <- c(
    "meters" = "Meters",
    "kilometers" = "Kilometers",
    "feet" = "Feet",
    "yards" = "Yards",
    "miles" = "Miles",
    "nautical_miles" = "NauticalMiles"
  )

  x <- rlang::arg_match(
    distance_units,
    names(lu),
    error_arg = error_arg,
    error_call = error_call
  )

  unname(lu[x])
}

validate_analysis_region <- function(
  analysis_region,
  error_arg = rlang::caller_arg(analysis_region),
  error_call = rlang::caller_call()
) {
  if (is.null(analysis_region)) {
    return(NULL)
  }
  lu <- setNames(
    c(
      "Europe",
      "Japan",
      "Korea",
      "MiddleEastAndAfrica",
      "NorthAmerica",
      "SouthAmerica",
      "SouthAsia",
      "Thailand"
    ),
    c(
      "europe",
      "japan",
      "korea",
      "middle_east_and_africa",
      "north_america",
      "south_america",
      "south_asia",
      "thailand"
    )
  )

  x <- rlang::arg_match(
    analysis_region,
    names(lu),
    error_arg = error_arg,
    error_call = error_call
  )

  unname(lu[x])
}

validate_u_turns_async <- function(
  u_turns,
  error_arg = rlang::caller_arg(u_turns),
  error_call = rlang::caller_call()
) {
  if (is.null(u_turns)) {
    return(NULL)
  }
  lu <- setNames(
    c(
      "Allowed",
      "Allowed only at Intersections and Dead Ends",
      "Allowed only at Dead Ends",
      "Not Allowed"
    ),
    c("allow_backtrack", "deadend_intersection", "deadend", "no_backtrack")
  )

  x <- rlang::arg_match(
    u_turns,
    names(lu),
    error_arg = error_arg,
    error_call = error_call
  )

  unname(lu[x])
}

validate_time_impedance <- function(
  time_impedance,
  error_arg = rlang::caller_arg(time_impedance),
  error_call = rlang::caller_call()
) {
  # Note the docs say
  # "These value are specific to the services published with the ArcGIS StreetMap Premium data. The values will be different if you are using other data for the analysis."
  # so this might be incorrect for other network analysis environment
  if (is.null(time_impedance)) {
    return(NULL)
  }

  lu <- setNames(
    c("Minutes", "TravelTime", "WalkTime", "TruckMinutes", "TruckTravelTime"),
    c(
      "minutes",
      "travel_time",
      "walk_time",
      "truck_minutes",
      "truck_travel_time"
    )
  )

  x <- rlang::arg_match(
    time_impedance,
    names(lu),
    error_arg = error_arg,
    error_call = error_call
  )

  unname(lu[x])
}

validate_distance_impedance <- function(
  distance_impedance,
  error_arg = rlang::caller_arg(distance_impedance),
  error_call = rlang::caller_call()
) {
  # Note the docs say
  # "These value are specific to the services published with the ArcGIS StreetMap Premium data. The values will be different if you are using other data for the analysis."
  # so this might be incorrect for other network analysis environment
  if (is.null(distance_impedance)) {
    return(NULL)
  }

  lu <- setNames(
    c("Miles", "Kilometers"),
    c("miles", "kilometers")
  )

  x <- rlang::arg_match(
    distance_impedance,
    names(lu),
    error_arg = error_arg,
    error_call = error_call
  )

  unname(lu[x])
}
