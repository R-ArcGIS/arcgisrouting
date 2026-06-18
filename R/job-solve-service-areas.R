.solve_service_areas_job <- R6::R6Class(
  "solve_service_areas_job",
  inherit = arcgisutils::arc_gp_job
)

#' Generate Service Areas (Async)
#'
#' Submits an asynchronous geoprocessing job to generate service areas around
#' facilities using the ArcGIS `/GenerateServiceAreas` GP service.
#'
#' @param facilities An `sf` or `sfc` object containing point geometries
#'   representing the facilities around which service areas are generated.
#' @param break_values Numeric vector. Service area sizes. Units are determined
#'   by `break_units`. Default: `NULL` (API default: `5 10 15`).
#' @param break_units Character. Units for `break_values`. One of:
#'   `"meters"`, `"kilometers"`, `"feet"`, `"yards"`, `"miles"`,
#'   `"nautical_miles"`, `"seconds"`, `"minutes"`, `"hours"`, `"days"`.
#'   Default: `NULL` (API default: `"minutes"`).
#' @param travel_direction Character. Direction of travel relative to
#'   facilities. One of: `"away"`, `"towards"`. Default: `NULL`
#'   (API default: `"away"`).
#' @param polygons_for_multiple_facilities Character. How service area polygons
#'   from multiple facilities are generated. One of: `"overlapping"`,
#'   `"not_overlapping"`, `"merge"`. Default: `NULL`
#'   (API default: `"overlapping"`).
#' @param polygon_overlap_type Character. Whether polygons are rings or disks.
#'   One of: `"rings"`, `"disks"`. Default: `NULL` (API default: `"rings"`).
#' @param detailed_polygons Logical. Generate detailed polygons. Default: `NULL`
#'   (API default: `FALSE`).
#' @param polygon_trim_distance List with elements `distance` (numeric) and
#'   `units` (character). Trim polygons to within this distance of the network.
#'   Default: `NULL`.
#' @param polygon_simplification_tolerance List with elements `distance`
#'   (numeric) and `units` (character). Simplification tolerance for output
#'   polygons. Default: `NULL`.
#' @param polygon_detail Character. Level of detail for output polygons. One
#'   of: `"standard"`, `"generalized"`, `"high"`. Default: `NULL`
#'   (API default: `"standard"`).
#' @param output_type Character. Type of output to generate. One of:
#'   `"polygons"`, `"lines"`, `"polygons_and_lines"`. Default: `NULL`
#'   (API default: `"polygons"`).
#' @param ignore_invalid_locations Logical. Whether to ignore invalid input
#'   locations. Default: `TRUE`.
#' @param output_format Character. Format for output features. One of:
#'   `"feature_set"`, `"json_file"`, `"geojson_file"`. Default:
#'   `"feature_set"`.
#' @inheritParams find_routes_job
#'
#' @returns A `solve_service_areas_job` R6 object inheriting from
#'   `arcgisutils::arc_gp_job`. Call `$start()` to submit and `$results` to
#'   retrieve output.
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(arcgisutils)
#' set_arc_token(auth_user())
#'
#' facilities <- st_sfc(
#'   st_point(c(-122.4194, 37.7749)),
#'   st_point(c(-122.0312, 37.3318)),
#'   crs = 4326
#' )
#'
#' job <- solve_service_areas_job(facilities, break_values = c(5, 10, 15))
#' job$start()
#' result <- job$results
#' }
#'
#' @family async
#' @family service area
#' @export
#' @references [API Reference](https://developers.arcgis.com/rest/routing/serviceArea-service-job/)
solve_service_areas_job <- function(
  facilities,
  break_values = NULL,
  break_units = NULL,
  travel_mode = NULL,
  travel_direction = NULL,
  time_of_day = NULL,
  use_hierarchy = NULL,
  uturn_at_junctions = NULL,
  polygons_for_multiple_facilities = NULL,
  polygon_overlap_type = NULL,
  detailed_polygons = NULL,
  polygon_trim_distance = NULL,
  polygon_simplification_tolerance = NULL,
  polygon_detail = NULL,
  output_type = NULL,
  analysis_region = NULL,
  restrictions = NULL,
  impedance = NULL,
  time_impedance = NULL,
  distance_impedance = NULL,
  ignore_invalid_locations = TRUE,
  output_format = "feature_set",
  point_barriers = NULL,
  line_barriers = NULL,
  polygon_barriers = NULL,
  token = arcgisutils::arc_token()
) {
  obj_check_token(token)
  facilities <- as_od_points(facilities)

  check_bool(use_hierarchy, allow_null = TRUE)
  check_bool(detailed_polygons, allow_null = TRUE)
  check_bool(ignore_invalid_locations)

  travel_mode <- validate_travel_mode(travel_mode, token = token)
  break_values <- validate_break_values(break_values)
  break_units <- validate_break_units(break_units)
  travel_direction <- validate_travel_direction(travel_direction)
  time_of_day <- validate_time_of_day(time_of_day)
  time_zone_for_time_of_day <- validate_tz_for_time_of_day(time_of_day)
  uturn_at_junctions <- validate_u_turns_async(uturn_at_junctions)
  polygons_for_multiple_facilities <- validate_multiple_facilities(
    polygons_for_multiple_facilities
  )
  polygon_overlap_type <- validate_overlap_type(polygon_overlap_type)
  polygon_trim_distance <- validate_tolerance(polygon_trim_distance)
  polygon_simplification_tolerance <- validate_tolerance(
    polygon_simplification_tolerance
  )
  polygon_detail <- validate_detail(polygon_detail)
  output_type <- validate_sa_output_type(output_type)
  analysis_region <- validate_analysis_region(analysis_region)
  restrictions <- validate_restrictions(restrictions)
  impedance <- validate_impedance_value(impedance)
  time_impedance <- validate_time_impedance(time_impedance)
  distance_impedance <- validate_distance_impedance(distance_impedance)
  output_format <- validate_job_output_format(output_format)

  point_barriers <- as_point_barriers(point_barriers)
  line_barriers <- as_polyline_barriers(line_barriers)
  polygon_barriers <- as_polygon_barriers(polygon_barriers)

  params <- compact(list(
    facilities = facilities,
    travel_mode = travel_mode,
    break_values = break_values,
    break_units = break_units,
    travel_direction = travel_direction,
    time_of_day = time_of_day,
    time_zone_for_time_of_day = time_zone_for_time_of_day,
    use_hierarchy = use_hierarchy,
    uturn_at_junctions = uturn_at_junctions,
    polygons_for_multiple_facilities = polygons_for_multiple_facilities,
    polygon_overlap_type = polygon_overlap_type,
    detailed_polygons = detailed_polygons,
    polygon_trim_distance = polygon_trim_distance,
    polygon_simplification_tolerance = polygon_simplification_tolerance,
    polygon_detail = polygon_detail,
    output_type = output_type,
    analysis_region = analysis_region,
    restrictions = restrictions,
    impedance = impedance,
    time_impedance = time_impedance,
    distance_impedance = distance_impedance,
    ignore_invalid_locations = ignore_invalid_locations,
    output_format = output_format,
    point_barriers = point_barriers,
    line_barriers = line_barriers,
    polygon_barriers = polygon_barriers,
    f = "json"
  ))

  meta <- arcgisutils::arc_portal_self(token)
  base_url <- meta$helperServices$asyncServiceArea$url

  if (is.null(base_url)) {
    cli::cli_abort("Cannot find async service area URL for this token")
  }

  .solve_service_areas_job$new(
    base_url,
    arcgisutils::as_form_params(params)@params,
    parse_solve_service_areas_results,
    token = token
  )
}

parse_solve_service_areas_results <- function(json) {
  compact(list(
    service_areas = try_parse(json, "/0/value"),
    solve_succeeded = RcppSimdJson::fparse(json, query = "/1/value"),
    facilities = try_parse(json, "/3/value"),
    service_area_lines = try_parse(json, "/4/value"),
    output_result_file = RcppSimdJson::fparse(json, query = "/5/value"),
    output_network_analysis_layer_package = RcppSimdJson::fparse(
      json,
      query = "/6/value"
    ),
    usage_cost = RcppSimdJson::fparse(json, query = "/7/value")
  ))
}


validate_sa_output_type <- function(
  x,
  error_arg = rlang::caller_arg(x),
  error_call = rlang::caller_call()
) {
  if (is.null(x)) return(NULL)

  lu <- c(
    "polygons" = "Polygons",
    "lines" = "Lines",
    "polygons_and_lines" = "Polygons and Lines"
  )

  x <- rlang::arg_match(
    x,
    names(lu),
    error_arg = error_arg,
    error_call = error_call
  )
  unname(lu[x])
}
