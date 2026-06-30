#' Route Vehicles
#'
#' Solves a vehicle routing problem (VRP) to find the most effective routes
#' for a fleet of vehicles visiting a set of orders.
#'
#' @param orders an `sf` or `sfc` object containing point geometries representing locations to visit.
#' @param depots an `sf` or `sfc` object containing point geometries representing depot locations.
#' @param routes default `NULL`. A `data.frame` describing vehicle and driver characteristics.
#' @param locate_settings default `NULL`. A list controlling how inputs are located on the network.
#' @param analysis_region default `NULL`. A scalar character. One of `"europe"`, `"japan"`,
#'   `"korea"`, `"middle_east_and_africa"`, `"north_america"`, `"south_america"`,
#'   `"south_asia"`, `"thailand"`. Speeds up analysis when specified.
#' @param time_zone_usage_for_time_fields default `NULL`. A scalar character. One of
#'   `"geo_local"` or `"utc"`. Time zone used for all date-time input fields.
#' @param default_date default `NULL`. A `POSIXt` or character scalar. The date on which
#'   all routes start. Only the date portion is used.
#' @param breaks default `NULL`. A `data.frame` of rest period definitions for routes.
#' @param time_units default `NULL`. A scalar character. One of `"seconds"`, `"minutes"`,
#'   `"hours"`, `"days"`. Units for all time-based attribute values.
#' @param distance_units default `NULL`. A scalar character. One of `"miles"`,
#'   `"kilometers"`, `"meters"`, `"feet"`, `"yards"`, `"nautical_miles"`. Units for all
#'   distance-based attribute values.
#' @param time_window_factor default `NULL`. A scalar character. One of `"low"`,
#'   `"medium"`, `"high"`. Importance of honoring time windows.
#' @param spatially_cluster_routes default `TRUE`. A logical scalar. Whether orders
#'   assigned to a route are spatially clustered.
#' @param route_zones default `NULL`. An `sf` or `sfc` polygon object delineating work
#'   territories for routes.
#' @param route_renewals default `NULL`. A `data.frame` of intermediate depot renewal
#'   locations for routes.
#' @param order_pairs default `NULL`. A `data.frame` pairing pickup and delivery orders.
#' @param excess_transit_factor default `NULL`. A scalar character. One of `"low"`,
#'   `"medium"`, `"high"`. Importance of reducing excess transit time for order pairs.
#' @param uturn_policy default `NULL`. A scalar character. One of `"allow_uturn"`,
#'   `"allow_dead_ends_and_intersections_only"`, `"allow_dead_ends_only"`, `"no_uturns"`.
#' @param use_hierarchy_in_analysis default `TRUE`. A logical scalar. Whether to use
#'   the street network hierarchy when finding routes.
#' @param attribute_parameter_values default `NULL`. A list of objects providing
#'   additional values required by an attribute or restriction.
#' @param time_impedance default `NULL`. A scalar character. Time-based impedance.
#'   One of `"travel_time"`, `"minutes"`, `"walk_time"`, `"truck_minutes"`,
#'   `"truck_travel_time"`.
#' @param distance_impedance default `NULL`. A scalar character. Distance-based
#'   impedance. One of `"miles"`, `"kilometers"`.
#' @param impedance default `NULL`. A scalar character. The impedance type. One of
#'   `"travel_time"`, `"minutes"`, `"truck_travel_time"`, `"truck_minutes"`,
#'   `"walk_time"`, `"miles"`, `"kilometers"`.
#' @param populate_route_lines default `TRUE`. A logical scalar. Whether output routes
#'   include the exact street shape.
#' @param route_line_simplification_tolerance default `NULL`. A numeric scalar.
#'   Simplification tolerance for route geometry.
#' @param populate_directions default `FALSE`. A logical scalar. Whether to generate
#'   driving directions for each route.
#' @param directions_language default `NULL`. A scalar character. Language code for
#'   directions (e.g. `"en"`).
#' @param directions_style_name default `NULL`. A scalar character. One of `"desktop"`,
#'   `"navigation"`, `"campus"`.
#' @param save_route_data default `FALSE`. A logical scalar. Whether to save results
#'   as a `.zip` file geodatabase.
#' @param save_output_layer default `FALSE`. A logical scalar. Whether to save the
#'   analysis as a network analysis layer package.
#' @param populate_stop_shapes default `FALSE`. A logical scalar. Whether output stops
#'   include point geometries.
#' @param ignore_invalid_order_locations default `FALSE`. A logical scalar. Whether to
#'   ignore invalid orders instead of failing.
#' @inheritParams find_routes
#' @inheritParams find_closest_facilities
#'
#' @returns A named list:
#' - `unassigned_stops`: orders that could not be assigned to any route
#' - `stops`: assigned stop features with route and sequence information
#' - `routes`: route features with geometry and cost attributes
#' - `directions`: driving directions (only when `populate_directions = TRUE`)
#' - `solve_succeeded`: logical indicating whether the solve completed
#' - `usage_cost`: list with `numObjects` and `credits` used
#' - `messages`: status and warning messages from the service
#'
#' @examples
#' \dontrun{
#' # This example is not executed since it requires a network connection
#' # to ArcGIS Online and a valid authentication token
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
#' routes <- data.frame(
#'   name = "Truck1",
#'   start_depot_name = "Depot1",
#'   end_depot_name = "Depot1",
#'   capacities = "40000"
#' )
#'
#' result <- route_vehicles(orders, depots, routes)
#' }
#'
#' @family direct
#' @family vrp
#' @export
#' @references [API Reference](https://developers.arcgis.com/rest/routing/vrp-service-direct/)
route_vehicles <- function(
  orders,
  depots,
  routes = NULL,
  travel_mode = NULL,
  locate_settings = NULL,
  analysis_region = NULL,
  time_zone_usage_for_time_fields = NULL,
  default_date = NULL,
  breaks = NULL,
  time_units = NULL,
  distance_units = NULL,
  time_window_factor = NULL,
  spatially_cluster_routes = TRUE,
  route_zones = NULL,
  route_renewals = NULL,
  order_pairs = NULL,
  excess_transit_factor = NULL,
  uturn_policy = NULL,
  point_barriers = NULL,
  line_barriers = NULL,
  polygon_barriers = NULL,
  use_hierarchy_in_analysis = TRUE,
  restrictions = NULL,
  attribute_parameter_values = NULL,
  time_impedance = NULL,
  distance_impedance = NULL,
  impedance = NULL,
  populate_route_lines = TRUE,
  route_line_simplification_tolerance = NULL,
  populate_directions = FALSE,
  directions_language = NULL,
  directions_style_name = NULL,
  save_route_data = FALSE,
  save_output_layer = FALSE,
  populate_stop_shapes = FALSE,
  ignore_invalid_order_locations = FALSE,
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

  orders <- as_stops(orders)
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

  default_date <- validate_time_of_day(default_date)
  travel_mode <- validate_travel_mode(travel_mode, token = token)
  restrictions <- validate_restrictions(restrictions)
  analysis_region <- validate_vrp_analysis_region(analysis_region)
  time_zone_usage_for_time_fields <- validate_time_zone_usage(
    time_zone_usage_for_time_fields
  )
  time_units <- validate_time_units(time_units)
  distance_units <- validate_distance_units_vrp(distance_units)
  time_window_factor <- validate_importance_factor(time_window_factor)
  excess_transit_factor <- validate_importance_factor(excess_transit_factor)
  uturn_policy <- validate_vrp_uturn_policy(uturn_policy)
  directions_style_name <- validate_directions_style_name(directions_style_name)
  time_impedance <- validate_impedance_value(time_impedance)
  distance_impedance <- validate_impedance_value(distance_impedance)
  impedance <- validate_impedance_value(impedance)

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
    travelMode = travel_mode,
    locateSettings = locate_settings,
    analysisRegion = analysis_region,
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
    attribute_parameter_values = attribute_parameter_values,
    time_impedance = time_impedance,
    distance_impedance = distance_impedance,
    impedance = impedance,
    populate_route_lines = populate_route_lines,
    route_line_simplification_tolerance = route_line_simplification_tolerance,
    populate_directions = populate_directions,
    directions_language = directions_language,
    directions_style_name = directions_style_name,
    save_route_data = save_route_data,
    save_output_layer = save_output_layer,
    populate_stop_shapes = populate_stop_shapes,
    ignore_invalid_order_locations = ignore_invalid_order_locations,
    f = "json"
  ))

  detect_errors(arcgisutils::arc_portal_self(token))

  service_url <- "https://logistics.arcgis.com/arcgis/rest/services/World/VehicleRoutingProblemSync/GPServer/EditVehicleRoutingProblem/execute"

  resp <- arcgisutils::arc_base_req(
    service_url,
    token,
    query = c("f" = "json")
  ) |>
    httr2::req_body_form(!!!params, .multi = "comma") |>
    httr2::req_perform() |>
    httr2::resp_body_string()

  if (grepl("err", substr(resp, 1, 5))) {
    yyjsonr::read_json_str(resp) |>
      detect_errors()
  }

  compact(list(
    unassigned_stops = try_parse(resp, query = "/results/0/value"),
    stops = try_parse(resp, query = "/results/1/value"),
    routes = try_parse(resp, query = "/results/2/value"),
    directions = try_parse(resp, query = "/results/3/value"),
    solve_succeeded = RcppSimdJson::fparse(resp, query = "/results/4/value"),
    usage_cost = RcppSimdJson::fparse(resp, query = "/results/9/value"),
    messages = RcppSimdJson::fparse(resp, query = "/messages")
  ))
}

as_route_zones <- function(x, ...) {
  UseMethod("as_route_zones")
}

#' @exportS3Method arcgisrouting::as_route_zones
as_route_zones.NULL <- function(x, ...) {
  NULL
}

#' @exportS3Method arcgisrouting::as_route_zones
as_route_zones.sfc <- function(x, ...) {
  if (!inherits(x, c("sfc_POLYGON", "sfc_MULTIPOLYGON"))) {
    cli::cli_abort(
      "Route zones must be polygon geometries, not {obj_type_friendly(x)}"
    )
  }
  if (is.na(sf::st_crs(x))) {
    cli::cli_abort(c("!" = "`crs` is not set. Please set the crs."))
  }
  arcgisutils::as_esri_features(x)
}

#' @exportS3Method arcgisrouting::as_route_zones
as_route_zones.sf <- function(x, ...) {
  geom <- sf::st_geometry(x)
  if (!inherits(geom, c("sfc_POLYGON", "sfc_MULTIPOLYGON"))) {
    cli::cli_abort(
      "Route zones must be polygon geometries, not {obj_type_friendly(geom)}"
    )
  }
  if (is.na(sf::st_crs(x))) {
    cli::cli_abort(c("!" = "`crs` is not set. Please set the crs."))
  }

  lu <- c("route_name" = "RouteName", "is_hard_zone" = "IsHardZone")
  common_cols <- intersect(names(lu), colnames(x))

  if (length(common_cols) == 0L) {
    return(as_route_zones(sf::st_geometry(x)))
  }

  for (col in common_cols) {
    target_col <- lu[[col]]
    if (target_col == "RouteName") {
      check_character(x[[col]], arg = col)
    }
    if (target_col == "IsHardZone") {
      check_logical(x[[col]], arg = col)
      x[[col]] <- as.integer(x[[col]])
    }
  }

  x <- x[common_cols]
  colnames(x) <- c(unname(lu[common_cols]), "geometry")
  sf::st_geometry(x) <- "geometry"

  arcgisutils::as_esri_features(x)
}

validate_vrp_uturn_policy <- function(
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
      "allow_uturn",
      "allow_dead_ends_and_intersections_only",
      "allow_dead_ends_only",
      "no_uturns"
    ),
    arg_nm = error_arg,
    error_call = error_call
  )

  lu <- c(
    "allow_uturn" = "ALLOW_UTURN",
    "allow_dead_ends_and_intersections_only" = "ALLOW_DEAD_ENDS_AND_INTERSECTIONS_ONLY",
    "allow_dead_ends_only" = "ALLOW_DEAD_ENDS_ONLY",
    "no_uturns" = "NO_UTURNS"
  )

  unname(lu[x])
}

validate_distance_units_vrp <- function(
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
      "meters",
      "feet",
      "yards",
      "nautical_miles"
    ),
    arg_nm = error_arg,
    error_call = error_call
  )

  lu <- c(
    "miles" = "Miles",
    "kilometers" = "Kilometers",
    "meters" = "Meters",
    "feet" = "Feet",
    "yards" = "Yards",
    "nautical_miles" = "NauticalMiles"
  )

  unname(lu[x])
}

validate_importance_factor <- function(
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
    values = c("low", "medium", "high"),
    arg_nm = error_arg,
    error_call = error_call
  )

  lu <- c("low" = "Low", "medium" = "Medium", "high" = "High")

  unname(lu[x])
}

validate_time_zone_usage <- function(
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
    values = c("geo_local", "utc"),
    arg_nm = error_arg,
    error_call = error_call
  )

  lu <- c("geo_local" = "GEO_LOCAL", "utc" = "UTC")

  unname(lu[x])
}

validate_vrp_analysis_region <- function(
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
      "europe",
      "japan",
      "korea",
      "middle_east_and_africa",
      "north_america",
      "south_america",
      "south_asia",
      "thailand"
    ),
    arg_nm = error_arg,
    error_call = error_call
  )

  lu <- c(
    "europe" = "Europe",
    "japan" = "Japan",
    "korea" = "Korea",
    "middle_east_and_africa" = "MiddleEastAndAfrica",
    "north_america" = "NorthAmerica",
    "south_america" = "SouthAmerica",
    "south_asia" = "SouthAsia",
    "thailand" = "Thailand"
  )

  unname(lu[x])
}
