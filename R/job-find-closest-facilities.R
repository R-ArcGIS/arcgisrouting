.find_closest_facilities_job <- R6::R6Class(
  "find_closest_facilities_job",
  inherit = arcgisutils::arc_gp_job
)

#' Find Closest Facilities (Async)
#'
#' Submits an asynchronous geoprocessing job that finds one or more nearby
#' facilities from incidents based on travel time or travel distance using the
#' ArcGIS `/FindClosestFacilities` GP service.
#'
#' @param incidents An `sf` or `sfc` object containing point geometries
#'   representing the locations from which the nearby facilities are searched.
#' @param facilities An `sf` or `sfc` object containing point geometries
#'   representing the locations that are searched for when finding the closest
#'   location.
#' @param number_of_facilities_to_find Integer. The number of closest
#'   facilities to find per incident. Default: `NULL` (API default: `1`).
#' @param cutoff Numeric. The travel time or travel distance value at which to
#'   stop searching for facilities for a given incident. Units are determined by
#'   `measurement_units`. Default: `NULL` (API default: no cutoff).
#' @param travel_direction Character. Direction the closest facility search is
#'   measured. One of: `"facility"` (from facilities to incidents) or
#'   `"incident"` (from incidents to facilities). Default: `NULL`
#'   (API default: `"incident"`).
#' @param time_of_day_usage Character. Whether `time_of_day` represents the
#'   departure or arrival time of the routes. One of: `"start"` (departure) or
#'   `"end"` (arrival). Default: `NULL` (API default: `"start"`).
#' @param route_line_simplification_tolerance List with elements `distance`
#'   (numeric) and `units` (character). Simplification tolerance for the output
#'   route geometry. Default: `NULL` (API default: `10` meters).
#' @param save_route_data Logical. Whether the route data is saved as a `.zip`
#'   file. Default: `NULL` (API default: `FALSE`).
#' @inheritParams find_routes_job
#' @inheritParams solve_service_areas_job
#'
#' @returns A `find_closest_facilities_job` R6 object inheriting from
#'   `arcgisutils::arc_gp_job`. Call `$start()` to submit and `$results` to
#'   retrieve output.
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
#' job <- find_closest_facilities_job(
#'   incidents,
#'   facilities,
#'   number_of_facilities_to_find = 2,
#'   travel_direction = "facility",
#'   cutoff = 5,
#'   measurement_units = "minutes",
#'   populate_directions = TRUE
#' )
#' job$start()
#' result <- job$results
#' }
#'
#' @family async
#' @family closest facility
#' @export
#' @references [API Reference](https://developers.arcgis.com/rest/routing/find-closest-facilities/)
find_closest_facilities_job <- function(
  incidents,
  facilities,
  travel_mode = NULL,
  number_of_facilities_to_find = NULL,
  cutoff = NULL,
  travel_direction = NULL,
  measurement_units = NULL,
  analysis_region = NULL,
  time_of_day = NULL,
  time_of_day_usage = NULL,
  uturn_at_junctions = NULL,
  use_hierarchy = NULL,
  restrictions = NULL,
  attribute_parameter_values = NULL,
  time_impedance = NULL,
  distance_impedance = NULL,
  impedance = NULL,
  point_barriers = NULL,
  line_barriers = NULL,
  polygon_barriers = NULL,
  route_shape = NULL,
  route_line_simplification_tolerance = NULL,
  populate_directions = NULL,
  directions_language = NULL,
  directions_distance_units = NULL,
  directions_style_name = NULL,
  save_route_data = NULL,
  save_output_network_analysis_layer = NULL,
  output_format = "feature_set",
  ignore_invalid_locations = TRUE,
  token = arcgisutils::arc_token()
) {
  obj_check_token(token)
  incidents <- as_incidents(incidents)
  facilities <- as_facilities(facilities)

  check_number_whole(number_of_facilities_to_find, allow_null = TRUE)
  check_number_decimal(cutoff, allow_null = TRUE)
  check_bool(use_hierarchy, allow_null = TRUE)
  check_bool(populate_directions, allow_null = TRUE)
  check_bool(save_route_data, allow_null = TRUE)
  check_bool(save_output_network_analysis_layer, allow_null = TRUE)
  check_bool(ignore_invalid_locations)
  check_string(directions_language, allow_null = TRUE)

  travel_mode <- validate_travel_mode(travel_mode, token = token)
  travel_direction <- validate_cf_travel_direction(travel_direction)
  measurement_units <- validate_measurement_units(measurement_units)
  analysis_region <- validate_analysis_region(analysis_region)
  time_of_day <- validate_time_of_day(time_of_day)
  time_zone_for_time_of_day <- validate_tz_for_time_of_day(time_of_day)
  time_of_day_usage <- validate_cf_time_of_day_usage(time_of_day_usage)
  uturn_at_junctions <- validate_u_turns_async(uturn_at_junctions)
  restrictions <- validate_restrictions(restrictions)
  impedance <- validate_impedance_value(impedance)
  time_impedance <- validate_time_impedance(time_impedance)
  distance_impedance <- validate_distance_impedance(distance_impedance)
  route_shape <- validate_route_shape(route_shape)
  route_line_simplification_tolerance <- validate_tolerance(
    route_line_simplification_tolerance
  )
  directions_distance_units <- validate_directions_distance_units(
    directions_distance_units
  )
  directions_style_name <- validate_directions_style_name(directions_style_name)
  output_format <- validate_job_output_format(output_format)

  point_barriers <- as_point_barriers(point_barriers)
  line_barriers <- as_polyline_barriers(line_barriers)
  polygon_barriers <- as_polygon_barriers(polygon_barriers)

  params <- compact(list(
    incidents = incidents,
    facilities = facilities,
    travel_mode = travel_mode,
    number_of_facilities_to_find = number_of_facilities_to_find,
    cutoff = cutoff,
    travel_direction = travel_direction,
    measurement_units = measurement_units,
    analysis_region = analysis_region,
    time_of_day = time_of_day,
    time_zone_for_time_of_day = time_zone_for_time_of_day,
    time_of_day_usage = time_of_day_usage,
    uturn_at_junctions = uturn_at_junctions,
    use_hierarchy = use_hierarchy,
    restrictions = restrictions,
    attribute_parameter_values = attribute_parameter_values,
    time_impedance = time_impedance,
    distance_impedance = distance_impedance,
    impedance = impedance,
    point_barriers = point_barriers,
    line_barriers = line_barriers,
    polygon_barriers = polygon_barriers,
    route_shape = route_shape,
    route_line_simplification_tolerance = route_line_simplification_tolerance,
    populate_directions = populate_directions,
    directions_language = directions_language,
    directions_distance_units = directions_distance_units,
    directions_style_name = directions_style_name,
    save_route_data = save_route_data,
    save_output_network_analysis_layer = save_output_network_analysis_layer,
    output_format = output_format,
    ignore_invalid_locations = ignore_invalid_locations,
    f = "json"
  ))

  meta <- arcgisutils::arc_portal_self(token)
  base_url <- meta$helperServices$asyncClosestFacility$url

  if (is.null(base_url)) {
    cli::cli_abort("Cannot find async closest facility URL for this token")
  }

  .find_closest_facilities_job$new(
    base_url,
    arcgisutils::as_form_params(params)@params,
    parse_find_closest_facilities_results,
    token = token
  )
}

parse_find_closest_facilities_results <- function(json) {
  compact(list(
    routes = try_parse(json, "/0/value"),
    directions = try_parse(json, "/1/value"),
    solve_succeeded = RcppSimdJson::fparse(json, query = "/2/value"),
    closest_facilities = try_parse(json, "/3/value"),
    output_network_analysis_layer = RcppSimdJson::fparse(
      json,
      query = "/4/value"
    ),
    output_route_data = RcppSimdJson::fparse(json, query = "/5/value"),
    incidents = try_parse(json, "/6/value"),
    facilities = try_parse(json, "/7/value"),
    output_result_file = RcppSimdJson::fparse(json, query = "/8/value"),
    output_network_analysis_layer_package = RcppSimdJson::fparse(
      json,
      query = "/9/value"
    ),
    direction_points = try_parse(json, "/10/value"),
    direction_lines = try_parse(json, "/11/value"),
    usage_cost = RcppSimdJson::fparse(json, query = "/12/value")
  ))
}


validate_cf_travel_direction <- function(
  x,
  error_arg = rlang::caller_arg(x),
  error_call = rlang::caller_call()
) {
  if (is.null(x)) {
    return(NULL)
  }

  lu <- c(
    "facility" = "Facility to Incident",
    "incident" = "Incident to Facility"
  )

  x <- rlang::arg_match(
    x,
    names(lu),
    error_arg = error_arg,
    error_call = error_call
  )
  unname(lu[x])
}


validate_cf_time_of_day_usage <- function(
  x,
  error_arg = rlang::caller_arg(x),
  error_call = rlang::caller_call()
) {
  if (is.null(x)) {
    return(NULL)
  }

  lu <- c(
    "start" = "Start Time",
    "end" = "End Time"
  )

  x <- rlang::arg_match(
    x,
    names(lu),
    error_arg = error_arg,
    error_call = error_call
  )
  unname(lu[x])
}


#' @noRd
#' @export
as_incidents <- function(x, ...) {
  UseMethod("as_incidents")
}

#' @export
as_incidents.sfc <- function(x, ...) {
  check_incidents(x)
  arcgisutils::as_esri_featureset(x)
}

check_incidents <- function(x, error_call = rlang::caller_env()) {
  if (!inherits(x, "sfc_POINT")) {
    cli::cli_abort(
      "Incidents must be point geometries not {obj_type_friendly(x)}",
      call = error_call
    )
  }

  if (is.na(sf::st_crs(x))) {
    cli::cli_abort(c("!" = "`crs` is not set. Please set the crs."))
  }

  n <- vctrs::vec_size(x)
  if (n > 5000L) {
    cli::cli_abort(
      c(
        "The maximum number of incidents supported is {.val {5000L}}",
        "i" = "Found {.val {n}} incidents"
      ),
      call = error_call
    )
  }
}

#' @export
as_incidents.sf <- function(x, verbose = TRUE, ...) {
  check_incidents(sf::st_geometry(x))

  lu <- c(
    "name" = "Name",
    "id" = "ID",
    "additional_time" = "AdditionalTime",
    "additional_distance" = "AdditionalDistance",
    "additional_cost" = "AdditionalCost",
    "cutoff" = "Cutoff",
    "target_facility_count" = "TargetFacilityCount",
    "curb_approach" = "CurbApproach",
    "bearing" = "Bearing",
    "bearing_tol" = "BearingTol",
    "nav_latency" = "NavLatency"
  )

  as_cf_featureset(x, lu, verbose, as_incidents)
}

#' @noRd
#' @export
as_facilities <- function(x, ...) {
  UseMethod("as_facilities")
}

#' @export
as_facilities.sfc <- function(x, ...) {
  check_facilities(x)
  arcgisutils::as_esri_featureset(x)
}

check_facilities <- function(x, error_call = rlang::caller_env()) {
  if (!inherits(x, "sfc_POINT")) {
    cli::cli_abort(
      "Facilities must be point geometries not {obj_type_friendly(x)}",
      call = error_call
    )
  }

  if (is.na(sf::st_crs(x))) {
    cli::cli_abort(c("!" = "`crs` is not set. Please set the crs."))
  }

  n <- vctrs::vec_size(x)
  if (n > 5000L) {
    cli::cli_abort(
      c(
        "The maximum number of facilities supported is {.val {5000L}}",
        "i" = "Found {.val {n}} facilities"
      ),
      call = error_call
    )
  }
}

#' @export
as_facilities.sf <- function(x, verbose = TRUE, ...) {
  check_facilities(sf::st_geometry(x))

  lu <- c(
    "name" = "Name",
    "id" = "ID",
    "additional_time" = "AdditionalTime",
    "additional_distance" = "AdditionalDistance",
    "additional_cost" = "AdditionalCost",
    "cutoff" = "Cutoff",
    "curb_approach" = "CurbApproach",
    "bearing" = "Bearing",
    "bearing_tol" = "BearingTol",
    "nav_latency" = "NavLatency"
  )

  as_cf_featureset(x, lu, verbose, as_facilities)
}

# Shared renamer/validator for closest facility incidents and facilities. Both
# locate types share these attribute checks; the per-type column lookup is the
# only difference and is passed in by the dispatching method.
as_cf_featureset <- function(x, lu, verbose, geometry_only) {
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

  string_cols <- c("Name", "ID")
  integer_cols <- c("TargetFacilityCount", "CurbApproach")

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

    if (
      target %in%
        c(
          "AdditionalTime",
          "AdditionalDistance",
          "AdditionalCost",
          "Cutoff",
          "Bearing",
          "BearingTol",
          "NavLatency"
        )
    ) {
      if (!is.numeric(x[[col]])) {
        cli::cli_abort("{.col {col}} must be a numeric vector")
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
