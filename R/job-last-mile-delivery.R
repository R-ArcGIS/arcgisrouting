.last_mile_delivery_job <- R6::R6Class(
  "last_mile_delivery_job",
  inherit = arcgisutils::arc_gp_job
)

#' Solve Last Mile Delivery (Async)
#'
#' Submits an asynchronous geoprocessing job to solve a last-mile delivery
#' problem using the ArcGIS `/SolveLastMileDelivery` GP service. Last Mile
#' Delivery is a specialized vehicle routing problem that creates
#' geographically clustered delivery routes to minimize fleet operating costs.
#'
#' @param orders An `sf` object containing point geometries representing
#'   delivery and pickup locations.
#' @param depots An `sf` or `sfc` object containing point geometries
#'   representing depot locations.
#' @param routes A `data.frame` describing vehicle and driver characteristics.
#'   Default: `NULL`.
#' @param order_specialties A `data.frame` mapping orders to the specialties
#'   they require, with columns `order_name` and `specialty_name`. Default:
#'   `NULL`.
#' @param route_specialties A `data.frame` mapping routes to the specialties
#'   they support, with columns `route_name` and `specialty_name`. Default:
#'   `NULL`.
#' @param zones An `sf` or `sfc` polygon object delineating work territories,
#'   each carrying a `name` attribute. Default: `NULL`.
#' @param ignore_network_location_fields Logical. Whether network location
#'   fields on the inputs are ignored. Default: `FALSE`.
#' @param earliest_route_start_date Date. Default earliest start date applied to
#'   routes whose `earliest_start_date` attribute is missing. Default: `NULL`.
#' @param earliest_route_start_time An `hms` or character `"hh:mm:ss"` value
#'   giving the default earliest start time applied to routes whose
#'   `earliest_start_time` attribute is missing. Default: `NULL`.
#' @param max_route_total_time Numeric. Default maximum allowed total time for
#'   each route, applied to routes whose `max_total_time` attribute is missing.
#'   Interpreted in `time_units`. Default: `NULL`.
#' @param sequence_gap Integer. Gap in numerical values to leave in the
#'   `Sequence` attribute between adjacent orders when the analysis is solved.
#'   Default: `NULL` (API default: `1`).
#' @param route_shape Character. Geometry type for output route lines. One of:
#'   `"true_shape_with_measures"`, `"straight_line"`, `"none"`. Default: `NULL`
#'   (API default: `"straight_line"`).
#' @param save_output_network_analysis_layer Logical. Whether the analysis
#'   settings are saved as a network analysis layer package file. Default:
#'   `FALSE`.
#' @inheritParams route_vehicles_job
#'
#' @returns A `last_mile_delivery_job` R6 object inheriting from
#'   `arcgisutils::arc_gp_job`. Call `$start()` to submit and `$results` to
#'   retrieve output.
#'
#' @examples
#' \dontrun{
#' # This example is not executed since it requires a network connection
#' # to ArcGIS Online and a valid authentication token
#' library(sf)
#' library(arcgisutils)
#' set_arc_token(auth_user())
#'
#' orders <- st_as_sf(
#'   data.frame(
#'     name = c("Order 1", "Order 2"),
#'     service_time = c(5, 5),
#'     time_window_start = as.POSIXct(
#'       c(NA, 1706860800),
#'       origin = "1970-01-01",
#'       tz = "UTC"
#'     ),
#'     time_window_end = as.POSIXct(
#'       c(1706868000, 1706868000),
#'       origin = "1970-01-01",
#'       tz = "UTC"
#'     ),
#'     max_violation_time = c(0, 30),
#'     delivery_quantity_1 = c(2000, 1500),
#'     delivery_quantity_2 = c(100, 75),
#'     x = c(-117, -117.5),
#'     y = c(34, 34.5)
#'   ),
#'   coords = c("x", "y"),
#'   crs = 4326
#' )
#'
#' depots <- st_as_sf(
#'   data.frame(name = "Depot 1", x = -117.2, y = 34.2),
#'   coords = c("x", "y"),
#'   crs = 4326
#' )
#'
#' routes <- data.frame(
#'   name = c("Truck 1", "Truck 2"),
#'   start_depot_name = c("Depot 1", "Depot 1"),
#'   end_depot_name = c("Depot 1", "Depot 1"),
#'   earliest_start_time = c("6:00:00", "6:00:00"),
#'   capacity_1 = c(40000, 30000),
#'   capacity_2 = c(2000, 2500),
#'   cost_per_unit_time = c(0.5, 0.5),
#'   cost_per_unit_distance = c(1.5, 1.5)
#' )
#'
#' order_specialties <- data.frame(
#'   order_name = c("Order 1", "Order 2"),
#'   specialty_name = c("Refrigerated", "Hazmat")
#' )
#'
#' route_specialties <- data.frame(
#'   route_name = c("Truck 1", "Truck 2"),
#'   specialty_name = c("Refrigerated", "Hazmat")
#' )
#'
#' zone1 <- st_polygon(list(rbind(
#'   c(-97.0634, 32.8442),
#'   c(-97.0554, 32.84),
#'   c(-97.0558, 32.8327),
#'   c(-97.0638, 32.83),
#'   c(-97.0634, 32.8442)
#' )))
#'
#' zone2 <- st_multipolygon(list(
#'   list(rbind(
#'     c(-97.0803, 32.8235),
#'     c(-97.0776, 32.8277),
#'     c(-97.074, 32.8254),
#'     c(-97.0767, 32.8227),
#'     c(-97.0803, 32.8235)
#'   )),
#'   list(rbind(
#'     c(-97.0871, 32.8311),
#'     c(-97.0831, 32.8292),
#'     c(-97.0853, 32.8259),
#'     c(-97.0892, 32.8279),
#'     c(-97.0871, 32.8311)
#'   ))
#' ))
#'
#' zones <- st_cast(
#'   st_sf(
#'     name = c("Zone 1", "Zone 2"),
#'     geometry = st_sfc(zone1, zone2, crs = 4326)
#'   ),
#'   "MULTIPOLYGON"
#' )
#'
#' job <- last_mile_delivery(
#'   orders = orders,
#'   depots = depots,
#'   routes = routes,
#'   order_specialties = order_specialties,
#'   route_specialties = route_specialties,
#'   zones = zones,
#'   earliest_route_start_date = as.Date("2024-02-02"),
#'   max_route_total_time = 480,
#'   sequence_gap = 3,
#'   time_units = "minutes",
#'   route_shape = "true_shape_with_measures",
#'   populate_directions = TRUE
#' )
#'
#' job$start()
#' job$results
#' }
#'
#' @family async
#' @family vrp
#' @export
#' @references [API Reference](https://developers.arcgis.com/rest/routing/last-mile-delivery-service/)
last_mile_delivery <- function(
  orders,
  depots,
  routes = NULL,
  order_specialties = NULL,
  route_specialties = NULL,
  zones = NULL,
  travel_mode = NULL,
  analysis_region = NULL,
  ignore_network_location_fields = FALSE,
  ignore_invalid_order_locations = FALSE,
  earliest_route_start_date = NULL,
  earliest_route_start_time = NULL,
  time_zone_usage_for_time_fields = NULL,
  max_route_total_time = NULL,
  sequence_gap = NULL,
  time_units = NULL,
  distance_units = NULL,
  route_shape = NULL,
  populate_directions = FALSE,
  directions_language = NULL,
  save_route_data = FALSE,
  save_output_network_analysis_layer = FALSE,
  output_format = "feature_set",
  point_barriers = NULL,
  line_barriers = NULL,
  polygon_barriers = NULL,
  token = arcgisutils::arc_token()
) {
  obj_check_token(token)

  check_bool(ignore_network_location_fields)
  check_bool(ignore_invalid_order_locations)
  check_bool(populate_directions)
  check_bool(save_route_data)
  check_bool(save_output_network_analysis_layer)
  check_string(directions_language, allow_null = TRUE)
  check_number_whole(sequence_gap, allow_null = TRUE)
  check_number_decimal(max_route_total_time, allow_null = TRUE, min = 0)

  orders <- as_lmd_orders(orders)
  depots <- as_stops(depots)
  routes <- as_lmd_routes(routes)
  order_specialties <- as_order_specialties(order_specialties)
  route_specialties <- as_route_specialties(route_specialties)
  zones <- as_lmd_zones(zones)

  travel_mode <- validate_travel_mode(travel_mode, token = token)
  analysis_region <- validate_analysis_region(analysis_region)
  time_zone_usage_for_time_fields <- validate_time_zone_usage(
    time_zone_usage_for_time_fields
  )
  earliest_route_start_date <- validate_start_date(earliest_route_start_date)
  earliest_route_start_time <- validate_start_time_hms(earliest_route_start_time)
  time_units <- validate_time_units(time_units)
  distance_units <- validate_distance_units_vrp(distance_units)
  route_shape <- validate_route_shape_lmd(route_shape)
  output_format <- validate_job_output_format(output_format)

  point_barriers <- as_point_barriers(point_barriers)
  line_barriers <- as_polyline_barriers(line_barriers)
  polygon_barriers <- as_polygon_barriers(polygon_barriers)

  params <- compact(list(
    orders = orders,
    depots = depots,
    routes = routes,
    order_specialties = order_specialties,
    route_specialties = route_specialties,
    zones = zones,
    travel_mode = travel_mode,
    analysis_region = analysis_region,
    ignore_network_location_fields = ignore_network_location_fields,
    ignore_invalid_order_locations = ignore_invalid_order_locations,
    earliest_route_start_date = earliest_route_start_date,
    earliest_route_start_time = earliest_route_start_time,
    time_zone_usage_for_time_fields = time_zone_usage_for_time_fields,
    max_route_total_time = max_route_total_time,
    sequence_gap = sequence_gap,
    time_units = time_units,
    distance_units = distance_units,
    point_barriers = point_barriers,
    line_barriers = line_barriers,
    polygon_barriers = polygon_barriers,
    route_shape = route_shape,
    populate_directions = populate_directions,
    directions_language = directions_language,
    save_route_data = save_route_data,
    save_output_network_analysis_layer = save_output_network_analysis_layer,
    output_format = output_format,
    f = "json"
  ))

  meta <- arcgisutils::arc_portal_self(token)
  vrp_task_url <- meta$helperServices$asyncVRP$url

  if (is.null(vrp_task_url)) {
    cli::cli_abort("Cannot find async VRP service URL for this token")
  }

  # asyncVRP points at the SolveVehicleRoutingProblem task, not the GPServer
  # root. Drop the trailing task segment then append SolveLastMileDelivery.
  base_url <- httr2::request(vrp_task_url) |>
    httr2::req_url_relative(".") |>
    httr2::req_url_path_append("SolveLastMileDelivery") |>
    _$url

  .last_mile_delivery_job$new(
    base_url,
    arcgisutils::as_form_params(params)@params,
    parse_last_mile_delivery_results,
    token = token
  )
}

parse_last_mile_delivery_results <- function(json) {
  compact(list(
    output_orders = try_parse(json, "/0/value"),
    output_routes = try_parse(json, "/1/value"),
    output_depots = try_parse(json, "/2/value"),
    output_depot_visits = try_parse(json, "/3/value"),
    output_direction_points = try_parse(json, "/4/value"),
    output_direction_lines = try_parse(json, "/5/value"),
    output_route_data = RcppSimdJson::fparse(json, query = "/6/value"),
    output_result_file = RcppSimdJson::fparse(json, query = "/7/value"),
    output_network_analysis_layer_package = RcppSimdJson::fparse(
      json,
      query = "/8/value"
    ),
    usage_cost = RcppSimdJson::fparse(json, query = "/9/value")
  ))
}

validate_route_shape_lmd <- function(
  x,
  error_arg = rlang::caller_arg(x),
  error_call = rlang::caller_call()
) {
  if (is.null(x)) {
    return(NULL)
  }

  check_string(x, arg = error_arg, call = error_call)

  lu <- c(
    "true_shape_with_measures" = "True Shape with Measures",
    "straight_line" = "Straight Line",
    "none" = "None"
  )

  x <- rlang::arg_match0(
    x,
    values = names(lu),
    arg_nm = error_arg,
    error_call = error_call
  )

  unname(lu[x])
}
