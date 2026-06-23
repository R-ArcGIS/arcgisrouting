.route_vehicles_job <- R6::R6Class(
  "route_vehicles_job",
  inherit = arcgisutils::arc_gp_job
)

#' Solve Vehicle Routing Problem (Async)
#'
#' Submits an asynchronous geoprocessing job to solve a vehicle routing problem
#' using the ArcGIS `/SolveVehicleRoutingProblem` GP service.
#'
#' @param orders An `sf` or `sfc` object containing point geometries
#'   representing locations to visit.
#' @param depots An `sf` or `sfc` object containing point geometries
#'   representing depot locations.
#' @param routes A `data.frame` describing vehicle and driver characteristics.
#'   Default: `NULL`.
#' @param breaks A `data.frame` of rest period definitions for routes.
#'   Default: `NULL`.
#' @param time_units Character. Units for all time-based attribute values. One
#'   of: `"seconds"`, `"minutes"`, `"hours"`, `"days"`. Default: `NULL`
#'   (API default: `"minutes"`).
#' @param distance_units Character. Units for all distance-based attribute
#'   values. One of: `"miles"`, `"kilometers"`, `"meters"`, `"feet"`,
#'   `"yards"`, `"nautical_miles"`. Default: `NULL` (API default: `"miles"`).
#' @param time_zone_usage_for_time_fields Character. Time zone used for all
#'   date-time input fields. One of: `"geo_local"`, `"utc"`. Default: `NULL`
#'   (API default: `"geo_local"`).
#' @param default_date POSIXct. The date on which all routes start. Only the
#'   date portion is used. Default: `NULL`.
#' @param time_window_factor Character. Importance of honoring time windows.
#'   One of: `"low"`, `"medium"`, `"high"`. Default: `NULL`
#'   (API default: `"medium"`).
#' @param spatially_cluster_routes Logical. Whether orders assigned to a route
#'   are spatially clustered. Default: `TRUE`.
#' @param route_zones An `sf` or `sfc` polygon object delineating work
#'   territories for routes. Default: `NULL`.
#' @param route_renewals A `data.frame` of intermediate depot renewal locations.
#'   Default: `NULL`.
#' @param order_pairs A `data.frame` pairing pickup and delivery orders.
#'   Default: `NULL`.
#' @param excess_transit_factor Character. Importance of reducing excess transit
#'   time for order pairs. One of: `"low"`, `"medium"`, `"high"`. Default:
#'   `NULL` (API default: `"medium"`).
#' @param uturn_policy Character. U-turn policy at junctions. One of:
#'   `"allow_uturn"`, `"allow_dead_ends_and_intersections_only"`,
#'   `"allow_dead_ends_only"`, `"no_uturns"`. Default: `NULL`.
#' @param use_hierarchy_in_analysis Logical. Whether to use the street network
#'   hierarchy when finding routes. Default: `TRUE`.
#' @param populate_route_lines Logical. Whether output routes include the exact
#'   street shape. Default: `TRUE`.
#' @param route_line_simplification_tolerance Numeric. Simplification tolerance
#'   for route geometry. Default: `NULL`.
#' @param populate_directions Logical. Whether to generate driving directions
#'   for each route. Default: `FALSE`.
#' @param directions_language Character. Language code for directions (e.g.
#'   `"en"`). Default: `NULL`.
#' @param directions_style_name Character. Formatting style for directions. One
#'   of: `"desktop"`, `"navigation"`, `"campus"`. Default: `NULL`.
#' @param save_route_data Logical. Whether to save results as a `.zip` file
#'   geodatabase. Default: `FALSE`.
#' @param save_output_layer Logical. Whether to save the analysis as a network
#'   analysis layer package. Default: `FALSE`.
#' @param populate_stop_shapes Logical. Whether output stops include point
#'   geometries. Default: `FALSE`.
#' @param ignore_invalid_order_locations Logical. Whether to ignore invalid
#'   orders instead of failing. Default: `FALSE`.
#' @inheritParams find_routes_job
#'
#' @returns A `route_vehicles_job` R6 object inheriting from
#'   `arcgisutils::arc_gp_job`. Call `$start()` to submit and `$results` to
#'   retrieve output.
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(arcgisutils)
#' set_arc_token(auth_user())
#'
#' orders <- st_sf(
#'   name = c("Order 1", "Order 2"),
#'   geometry = st_sfc(
#'     st_point(c(-0.1891, 51.5254)),
#'     st_point(c(-0.1744, 51.5353)),
#'     crs = 4326
#'   )
#' )
#'
#' depots <- st_sf(
#'   name = "Depot1",
#'   geometry = st_sfc(st_point(c(-0.2, 51.5)), crs = 4326)
#' )
#'
#' job <- route_vehicles_job(orders, depots)
#' job$start()
#' result <- job$results
#' }
#'
#' @family async
#' @family vrp
#' @export
#' @references [API Reference](https://developers.arcgis.com/rest/routing/vrp-service-job/)
route_vehicles_job <- function(
  orders,
  depots,
  routes = NULL,
  breaks = NULL,
  travel_mode = NULL,
  analysis_region = NULL,
  time_zone_usage_for_time_fields = NULL,
  default_date = NULL,
  time_units = NULL,
  distance_units = NULL,
  time_window_factor = NULL,
  spatially_cluster_routes = TRUE,
  route_zones = NULL,
  route_renewals = NULL,
  order_pairs = NULL,
  excess_transit_factor = NULL,
  uturn_policy = NULL,
  restrictions = NULL,
  impedance = NULL,
  time_impedance = NULL,
  distance_impedance = NULL,
  use_hierarchy_in_analysis = TRUE,
  populate_route_lines = TRUE,
  route_line_simplification_tolerance = NULL,
  populate_directions = FALSE,
  directions_language = NULL,
  directions_style_name = NULL,
  save_route_data = FALSE,
  save_output_layer = FALSE,
  populate_stop_shapes = FALSE,
  output_format = "feature_set",
  ignore_invalid_order_locations = FALSE,
  point_barriers = NULL,
  line_barriers = NULL,
  polygon_barriers = NULL,
  token = arcgisutils::arc_token()
) {
  obj_check_token(token)

  check_bool(spatially_cluster_routes)
  check_bool(use_hierarchy_in_analysis)
  check_bool(populate_route_lines)
  check_bool(populate_directions)
  check_bool(save_route_data)
  check_bool(save_output_layer)
  check_bool(populate_stop_shapes)
  check_bool(ignore_invalid_order_locations)
  check_number_decimal(route_line_simplification_tolerance, allow_null = TRUE)
  check_string(directions_language, allow_null = TRUE)

  orders <- as_vrp_orders(orders)
  depots <- as_stops(depots)
  routes <- as_routes(routes)

  breaks <- if (!is.null(breaks)) arcgisutils::as_esri_featureset(breaks)
  route_renewals <- if (!is.null(route_renewals)) {
    arcgisutils::as_esri_featureset(route_renewals)
  }
  order_pairs <- if (!is.null(order_pairs)) {
    arcgisutils::as_esri_featureset(order_pairs)
  }
  route_zones <- as_route_zones(route_zones)

  travel_mode <- validate_travel_mode(travel_mode, token = token)
  analysis_region <- validate_analysis_region(analysis_region)
  time_zone_usage_for_time_fields <- validate_time_zone_usage(
    time_zone_usage_for_time_fields
  )
  default_date <- validate_time_of_day(default_date)
  time_units <- validate_time_units(time_units)
  distance_units <- validate_distance_units_vrp(distance_units)
  time_window_factor <- validate_importance_factor(time_window_factor)
  excess_transit_factor <- validate_importance_factor(excess_transit_factor)
  uturn_policy <- validate_vrp_uturn_policy(uturn_policy)
  restrictions <- validate_restrictions(restrictions)
  impedance <- validate_impedance_value(impedance)
  time_impedance <- validate_time_impedance(time_impedance)
  distance_impedance <- validate_distance_impedance(distance_impedance)
  directions_style_name <- validate_directions_style_name(directions_style_name)
  output_format <- validate_job_output_format(output_format)

  point_barriers <- as_point_barriers(point_barriers)
  line_barriers <- as_polyline_barriers(line_barriers)
  polygon_barriers <- as_polygon_barriers(polygon_barriers)

  params <- compact(list(
    orders = orders,
    depots = depots,
    routes = routes,
    breaks = breaks,
    route_zones = route_zones,
    route_renewals = route_renewals,
    order_pairs = order_pairs,
    travel_mode = travel_mode,
    analysis_region = analysis_region,
    time_zone_usage_for_time_fields = time_zone_usage_for_time_fields,
    default_date = default_date,
    time_units = time_units,
    distance_units = distance_units,
    time_window_factor = time_window_factor,
    spatially_cluster_routes = spatially_cluster_routes,
    excess_transit_factor = excess_transit_factor,
    uturn_policy = uturn_policy,
    point_barriers = point_barriers,
    line_barriers = line_barriers,
    polygon_barriers = polygon_barriers,
    use_hierarchy_in_analysis = use_hierarchy_in_analysis,
    restrictions = restrictions,
    impedance = impedance,
    time_impedance = time_impedance,
    distance_impedance = distance_impedance,
    populate_route_lines = populate_route_lines,
    route_line_simplification_tolerance = route_line_simplification_tolerance,
    populate_directions = populate_directions,
    directions_language = directions_language,
    directions_style_name = directions_style_name,
    save_route_data = save_route_data,
    save_output_layer = save_output_layer,
    populate_stop_shapes = populate_stop_shapes,
    output_format = output_format,
    ignore_invalid_order_locations = ignore_invalid_order_locations,
    f = "json"
  ))

  meta <- arcgisutils::arc_portal_self(token)
  base_url <- meta$helperServices$asyncVRP$url

  if (is.null(base_url)) {
    cli::cli_abort("Cannot find async VRP service URL for this token")
  }

  .route_vehicles_job$new(
    base_url,
    arcgisutils::as_form_params(params)@params,
    parse_route_vehicles_results,
    token = token
  )
}

parse_route_vehicles_results <- function(json) {
  compact(list(
    out_unassigned_stops = try_parse(json, "/0/value"),
    out_stops = try_parse(json, "/1/value"),
    out_routes = try_parse(json, "/2/value"),
    out_directions = try_parse(json, "/3/value"),
    solve_succeeded = RcppSimdJson::fparse(json, query = "/4/value"),
    out_network_analysis_layer = RcppSimdJson::fparse(json, query = "/5/value"),
    out_route_data = RcppSimdJson::fparse(json, query = "/6/value"),
    out_result_file = RcppSimdJson::fparse(json, query = "/7/value"),
    output_network_analysis_layer_package = RcppSimdJson::fparse(
      json,
      query = "/8/value"
    ),
    usage_cost = RcppSimdJson::fparse(json, query = "/9/value")
  ))
}
