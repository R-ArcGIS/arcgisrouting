.location_allocation_job <- R6::R6Class(
  "location_allocation_job",
  inherit = arcgisutils::arc_gp_job
)

#' Solve Location-Allocation (Async)
#'
#' Submits an asynchronous geoprocessing job that chooses the set of facilities
#' which best serve demand from surrounding areas, simultaneously locating
#' facilities and allocating demand points to them, using the ArcGIS
#' `/SolveLocationAllocation` GP service.
#'
#' @param facilities An `sf` or `sfc` object containing point geometries
#'   representing the locations that serve as facilities.
#' @param demand_points An `sf` or `sfc` object containing point geometries
#'   representing the demand points.
#' @param locate_settings List. Settings that affect how input data are located,
#'   passed through as a JSON object. Default: `NULL`.
#' @param problem_type Character. Objective of the location-allocation analysis.
#'   One of: `"maximize_attendance"`, `"maximize_capacitated_coverage"`,
#'   `"maximize_coverage"`, `"maximize_market_share"`, `"minimize_facilities"`,
#'   `"minimize_impedance"`, `"target_market_share"`. Default: `NULL`
#'   (API default: `"minimize_impedance"`).
#' @param number_of_facilities_to_find Integer. The number of facilities the
#'   task should choose. Default: `NULL` (API default: `1`).
#' @param default_measurement_cutoff Numeric. The maximum travel time or distance
#'   allowed between a demand point and the facility to which it is allocated.
#'   Units are determined by `measurement_units`. Default: `NULL` (API default:
#'   no cutoff).
#' @param default_capacity Numeric. The default capacity assigned to all
#'   facilities. Only applicable to the `"maximize_capacitated_coverage"`
#'   problem type. Default: `NULL` (API default: `1`).
#' @param target_market_share Numeric. The percentage of total demand weight to
#'   capture. Only applicable to the `"target_market_share"` problem type.
#'   Default: `NULL` (API default: `10`).
#' @param measurement_transformation_model Character. Equation for transforming
#'   the network cost between facilities and demand points. One of: `"linear"`,
#'   `"power"`, `"exponential"`. Default: `NULL` (API default: `"linear"`).
#' @param measurement_transformation_factor Numeric. The impedance parameter
#'   value (lambda) for `measurement_transformation_model`. Ignored when the
#'   model is linear. Default: `NULL` (API default: `1`).
#' @param travel_direction Character. Direction in which to measure travel. One
#'   of: `"facility_to_demand"` or `"demand_to_facility"`. Default: `NULL`
#'   (API default: `"facility_to_demand"`).
#' @param allocation_line_shape Character. Type of line features output by the
#'   request. One of: `"straight_line"` or `"none"`. Default: `NULL`
#'   (API default: `"straight_line"`).
#' @param attribute_parameter_values List. Additional values for attributes or restrictions, passed through as a JSON object. Default: `NULL`.
#' @param save_output_network_analysis_layer Logical. Whether to save the analysis as a network analysis layer package file. Default: `NULL` (API default: `FALSE`).
#' @inheritParams find_routes_job
#' @inheritParams find_service_areas_job
#'
#' @returns A `location_allocation_job` R6 object inheriting from
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
#' facilities <- st_sf(
#'   name = c("Facility A", "Facility B"),
#'   facility_type = c(0L, 0L),
#'   curb_approach = c(0L, 0L),
#'   geometry = st_sfc(
#'     st_point(c(-58.557329417999938, -34.587693706999971)),
#'     st_point(c(-58.460247408999976, -34.683348039999942)),
#'     crs = 4326
#'   )
#' )
#'
#' demand_points <- st_sf(
#'   name = c("Household 4", "Household 3", "Household 2", "Household 1"),
#'   group_name = c("A", "A", NA, NA),
#'   weight = c(2, 2, 3, 5),
#'   curb_approach = c(0L, 0L, 0L, 1L),
#'   geometry = st_sfc(
#'     st_point(c(-58.664405163999959, -34.614819562999969)),
#'     st_point(c(-58.514499119999982, -34.496322404999944)),
#'     st_point(c(-58.54162497599998, -34.788996107999935)),
#'     st_point(c(-58.40599569799997, -34.637662387999967)),
#'     crs = 4326
#'   )
#' )
#'
#' job <- location_allocation_job(facilities, demand_points)
#' job$start()
#' result <- job$results
#' }
#'
#' @family async
#' @family location-allocation
#' @export
#' @references [API Reference](https://developers.arcgis.com/rest/routing/locationAllocation-service-job/)
location_allocation_job <- function(
  facilities,
  demand_points,
  travel_mode = NULL,
  locate_settings = NULL,
  measurement_units = NULL,
  analysis_region = NULL,
  problem_type = NULL,
  number_of_facilities_to_find = NULL,
  default_measurement_cutoff = NULL,
  default_capacity = NULL,
  target_market_share = NULL,
  measurement_transformation_model = NULL,
  measurement_transformation_factor = NULL,
  travel_direction = NULL,
  time_of_day = NULL,
  uturn_at_junctions = NULL,
  use_hierarchy = NULL,
  restrictions = NULL,
  attribute_parameter_values = NULL,
  allocation_line_shape = NULL,
  time_impedance = NULL,
  distance_impedance = NULL,
  impedance = NULL,
  point_barriers = NULL,
  line_barriers = NULL,
  polygon_barriers = NULL,
  save_output_network_analysis_layer = NULL,
  output_format = "feature_set",
  ignore_invalid_locations = TRUE,
  token = arcgisutils::arc_token()
) {
  obj_check_token(token)
  facilities <- as_la_facilities(facilities)
  demand_points <- as_demand_points(demand_points)

  check_number_whole(number_of_facilities_to_find, allow_null = TRUE)
  check_number_decimal(default_measurement_cutoff, allow_null = TRUE)
  check_number_decimal(default_capacity, allow_null = TRUE)
  check_number_decimal(target_market_share, allow_null = TRUE)
  check_number_decimal(measurement_transformation_factor, allow_null = TRUE)
  check_bool(use_hierarchy, allow_null = TRUE)
  check_bool(save_output_network_analysis_layer, allow_null = TRUE)
  check_bool(ignore_invalid_locations)

  travel_mode <- validate_travel_mode(travel_mode, token = token)
  measurement_units <- validate_measurement_units(measurement_units)
  analysis_region <- validate_analysis_region(analysis_region)
  problem_type <- validate_problem_type(problem_type)
  measurement_transformation_model <- validate_transformation_model(
    measurement_transformation_model
  )
  travel_direction <- validate_la_travel_direction(travel_direction)
  time_of_day <- validate_time_of_day(time_of_day)
  time_zone_for_time_of_day <- validate_tz_for_time_of_day(time_of_day)
  uturn_at_junctions <- validate_u_turns_async(uturn_at_junctions)
  restrictions <- validate_restrictions(restrictions)
  allocation_line_shape <- validate_allocation_line_shape(allocation_line_shape)
  time_impedance <- validate_time_impedance(time_impedance)
  distance_impedance <- validate_distance_impedance(distance_impedance)
  impedance <- validate_impedance_value(impedance)
  output_format <- validate_job_output_format(output_format)

  point_barriers <- as_point_barriers(point_barriers)
  line_barriers <- as_polyline_barriers(line_barriers)
  polygon_barriers <- as_polygon_barriers(polygon_barriers)

  params <- compact(list(
    facilities = facilities,
    demand_points = demand_points,
    travel_mode = travel_mode,
    locate_settings = locate_settings,
    measurement_units = measurement_units,
    analysis_region = analysis_region,
    problem_type = problem_type,
    number_of_facilities_to_find = number_of_facilities_to_find,
    default_measurement_cutoff = default_measurement_cutoff,
    default_capacity = default_capacity,
    target_market_share = target_market_share,
    measurement_transformation_model = measurement_transformation_model,
    measurement_transformation_factor = measurement_transformation_factor,
    travel_direction = travel_direction,
    time_of_day = time_of_day,
    time_zone_for_time_of_day = time_zone_for_time_of_day,
    uturn_at_junctions = uturn_at_junctions,
    use_hierarchy = use_hierarchy,
    restrictions = restrictions,
    attribute_parameter_values = attribute_parameter_values,
    allocation_line_shape = allocation_line_shape,
    time_impedance = time_impedance,
    distance_impedance = distance_impedance,
    impedance = impedance,
    point_barriers = point_barriers,
    line_barriers = line_barriers,
    polygon_barriers = polygon_barriers,
    save_output_network_analysis_layer = save_output_network_analysis_layer,
    output_format = output_format,
    ignore_invalid_locations = ignore_invalid_locations,
    f = "json"
  ))

  meta <- arcgisutils::arc_portal_self(token)
  base_url <- meta$helperServices$asyncLocationAllocation$url

  if (is.null(base_url)) {
    cli::cli_abort("Cannot find async location-allocation URL for this token")
  }

  job_url <- httr2::req_url_path_append(
    httr2::request(base_url),
    "SolveLocationAllocation"
  )$url

  .location_allocation_job$new(
    job_url,
    arcgisutils::as_form_params(params)@params,
    parse_location_allocation_results,
    token = token
  )
}

parse_location_allocation_results <- function(json) {
  compact(list(
    solve_succeeded = RcppSimdJson::fparse(json, query = "/0/value"),
    allocation_lines = try_parse(json, "/1/value"),
    facilities = try_parse(json, "/2/value"),
    demand_points = try_parse(json, "/3/value"),
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


validate_problem_type <- function(
  x,
  error_arg = rlang::caller_arg(x),
  error_call = rlang::caller_call()
) {
  if (is.null(x)) {
    return(NULL)
  }

  lu <- c(
    "maximize_attendance" = "Maximize Attendance",
    "maximize_capacitated_coverage" = "Maximize Capacitated Coverage",
    "maximize_coverage" = "Maximize Coverage",
    "maximize_market_share" = "Maximize Market Share",
    "minimize_facilities" = "Minimize Facilities",
    "minimize_impedance" = "Minimize Impedance",
    "target_market_share" = "Target Market Share"
  )

  x <- rlang::arg_match(
    x,
    names(lu),
    error_arg = error_arg,
    error_call = error_call
  )
  unname(lu[x])
}


validate_transformation_model <- function(
  x,
  error_arg = rlang::caller_arg(x),
  error_call = rlang::caller_call()
) {
  if (is.null(x)) {
    return(NULL)
  }

  lu <- c(
    "linear" = "Linear",
    "power" = "Power",
    "exponential" = "Exponential"
  )

  x <- rlang::arg_match(
    x,
    names(lu),
    error_arg = error_arg,
    error_call = error_call
  )
  unname(lu[x])
}


validate_la_travel_direction <- function(
  x,
  error_arg = rlang::caller_arg(x),
  error_call = rlang::caller_call()
) {
  if (is.null(x)) {
    return(NULL)
  }

  lu <- c(
    "facility_to_demand" = "Facility to Demand",
    "demand_to_facility" = "Demand to Facility"
  )

  x <- rlang::arg_match(
    x,
    names(lu),
    error_arg = error_arg,
    error_call = error_call
  )
  unname(lu[x])
}


validate_allocation_line_shape <- function(
  x,
  error_arg = rlang::caller_arg(x),
  error_call = rlang::caller_call()
) {
  if (is.null(x)) {
    return(NULL)
  }

  lu <- c(
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


#' Convert spatial objects to facilities or demand points
#'
#' @noRd
as_la_facilities <- function(x, ...) {
  UseMethod("as_la_facilities")
}

#' @export
as_la_facilities.sfc <- function(x, ...) {
  check_la_points(x, "Facilities")
  arcgisutils::as_esri_featureset(x)
}

#' @export
as_la_facilities.sf <- function(x, verbose = TRUE, ...) {
  check_la_points(sf::st_geometry(x), "Facilities")

  lu <- c(
    "name" = "Name",
    "facility_type" = "FacilityType",
    "weight" = "Weight",
    "cutoff" = "Cutoff",
    "capacity" = "Capacity",
    "curb_approach" = "CurbApproach",
    "bearing" = "Bearing",
    "bearing_tol" = "BearingTol",
    "nav_latency" = "NavLatency"
  )

  as_la_featureset(x, lu, verbose, as_la_facilities)
}

#' @noRd
as_demand_points <- function(x, ...) {
  UseMethod("as_demand_points")
}

#' @export
as_demand_points.sfc <- function(x, ...) {
  check_la_points(x, "Demand points")
  arcgisutils::as_esri_featureset(x)
}

#' @export
as_demand_points.sf <- function(x, verbose = TRUE, ...) {
  check_la_points(sf::st_geometry(x), "Demand points")

  lu <- c(
    "name" = "Name",
    "group_name" = "GroupName",
    "weight" = "Weight",
    "cutoff" = "Cutoff",
    "impedance_transformation" = "ImpedanceTransformation",
    "impedance_parameter" = "ImpedanceParameter",
    "curb_approach" = "CurbApproach",
    "bearing" = "Bearing",
    "bearing_tol" = "BearingTol",
    "nav_latency" = "NavLatency"
  )

  as_la_featureset(x, lu, verbose, as_demand_points)
}

check_la_points <- function(x, label, error_call = rlang::caller_call()) {
  if (!inherits(x, "sfc_POINT")) {
    cli::cli_abort(
      "{label} must be point geometries not {obj_type_friendly(x)}",
      call = error_call
    )
  }

  if (is.na(sf::st_crs(x))) {
    cli::cli_abort(c("!" = "`crs` is not set. Please set the crs."))
  }
}

# Shared renamer/validator for location-allocation facilities and demand points.
# The per-type column lookup is the only difference and is passed in by the
# dispatching method.
as_la_featureset <- function(x, lu, verbose, geometry_only) {
  common_lu_vals <- na.omit(lu[colnames(x)])
  common_cols <- names(common_lu_vals)
  n_common <- length(common_lu_vals)

  if (verbose && n_common > 0) {
    cli::cli_alert_info("Using the provided attributes: {.col {common_cols}}")
  }

  if (n_common == 0) {
    return(geometry_only(sf::st_geometry(x)))
  }

  n_col <- table(common_lu_vals)
  are_dupes <- n_col > 1
  if (any(are_dupes)) {
    cli::cli_abort(
      "Found duplicate column{?s}: {.col {names(n_col)[are_dupes]}}"
    )
  }

  string_cols <- c("Name", "GroupName")
  integer_cols <- c("FacilityType", "ImpedanceTransformation", "CurbApproach")
  numeric_cols <- c(
    "Weight",
    "Cutoff",
    "Capacity",
    "ImpedanceParameter",
    "Bearing",
    "BearingTol",
    "NavLatency"
  )

  for (col in common_cols) {
    target <- lu[[col]]

    if (target %in% string_cols) {
      check_character(x[[col]], arg = col)
    }

    if (target %in% integer_cols) {
      if (!rlang::is_integerish(x[[col]])) {
        cli::cli_abort("{.col {col}} must be a whole number vector")
      }
      x[[col]] <- as.integer(x[[col]])
    }

    if (target %in% numeric_cols) {
      if (!is.numeric(x[[col]])) {
        cli::cli_abort("{.col {col}} must be a numeric vector")
      }
    }

    if (target == "FacilityType") {
      rng <- range(x[[col]], na.rm = TRUE)
      if (rng[1] < 0 || rng[2] > 2) {
        cli::cli_abort("{.col {col}} must be between 0 and 2 inclusive")
      }
    }

    if (target == "CurbApproach") {
      rng <- range(x[[col]], na.rm = TRUE)
      if (rng[1] < 0 || rng[2] > 3) {
        cli::cli_abort("{.col {col}} must be between 0 and 3 inclusive")
      }
    }

    if (target == "BearingTol") {
      rng <- range(x[[col]], na.rm = TRUE)
      if (rng[1] < 0 || rng[2] > 180) {
        cli::cli_abort("{.col {col}} must be between 0 and 180 inclusive")
      }
    }
  }

  if (
    "NavLatency" %in%
      common_lu_vals &&
      !("Bearing" %in% common_lu_vals && "BearingTol" %in% common_lu_vals)
  ) {
    cli::cli_abort(
      "{.col nav_latency} provided but not accompanied by {.col bearing} and {.col bearing_tol}"
    )
  }

  x <- x[common_cols]
  colnames(x) <- c(unname(common_lu_vals), "geometry")
  sf::st_geometry(x) <- "geometry"

  arcgisutils::as_esri_featureset(x)
}
