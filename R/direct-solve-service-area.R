#' Solve Service Area
#'
#'
#' @param facilities an `sf` or `sfc` object containing point geometries representing the facilities around which service areas are generated.
#' @param default_breaks default `c(5, 10, 15)`. A numeric vector specifying the size and number of service areas to generate for each facility. Units are determined by the `impedance` parameter.
#' @param travel_direction default `"away"`. A scalar character. One of `"away"` (away from facility) or `"towards"` (toward facility).
#' @param output_polygons default `"simplified"`. A scalar character or `NULL` (no polygons). One of `"simplified"` or `"detailed"`.
#' @param split_polygons_at_breaks default `TRUE`. A logical scalar. When `TRUE`, service areas appear as rings between breaks. When `FALSE`, each area is a disk from the facility to the break.
#' @param overlap_polygons default `TRUE`. A logical scalar. Whether service area polygons from different facilities can overlap.
#' @param merge_similar_polygon_ranges default `FALSE`. A logical scalar. Whether service area polygons from different facilities with the same break value are merged into a single polygon.
#' @param trim_outer_polygons default `TRUE`. A logical scalar. Whether service areas are trimmed to lie within a distance of the network. Ignored when `use_hierarchy = TRUE`.
#' @param trim_polygon_distance default `100`. An integer scalar. The distance within which the service area polygon extends from the network.
#' @param trim_polygon_distance_units default `"meters"`. A scalar character. Units for `trim_polygon_distance`. One of `"meters"`, `"kilometers"`, `"feet"`, `"miles"`, `"nautical_miles"`, `"yards"`.
#' @param output_lines default `NULL` (no lines). A scalar character or `NULL`. One of `"true_shape"` or `"with_measure"`.
#' @param split_lines_at_breaks default `TRUE`. A logical scalar. Whether service area lines are split at break values.
#' @param overlap_lines default `TRUE`. A logical scalar. Whether service area lines from different facilities can overlap.
#' @param return_geometry default `"service_areas"`. A character vector. Valid values: `"service_areas"`, `"sa_lines"`, `"facilities"`, `"barriers"`, `"polyline_barriers"`, `"polygon_barriers"`.
#' @param output_geometry_precision default `10`. A numeric scalar. Simplification tolerance applied to output geometry.
#' @param output_geometry_precision_units default `"meters"`. A scalar character. Units for `output_geometry_precision`. Same valid values as `trim_polygon_distance_units`.
#' @param crs default `4326`. The coordinate reference system of the output geometries. Passed to `arcgisutils::as_spatial_reference()`.
#' @param time_of_day default `NULL`. A scalar date-time. Either a `POSIXt` scalar or a character string parseable by `as.POSIXlt()`. The time and date at which travel begins.
#' @param u_turns default `NULL`. A scalar character. U-turn policy at junctions. One of `"allow_backtrack"`, `"deadend_intersection"`, `"deadend"`, `"no_backtrack"`.
#' @param impedance default `NULL`. A scalar character. The impedance to minimize. One of `"travel_time"`, `"minutes"`, `"truck_travel_time"`, `"truck_minutes"`, `"walk_time"`, `"miles"`, `"kilometers"`.
#' @param accumulate_impedance default `NULL`. A character vector. Additional impedance values to accumulate.
#' @param point_barriers default `NULL`. An `sf` or `sfc` object of point geometries representing barriers to restrict or add cost to travel.
#' @param line_barriers default `NULL`. An `sf` or `sfc` object of line geometries representing barriers to restrict or add cost to travel.
#' @inheritParams find_routes
#'
#' @returns A list containing the service area results.
#'
#' @examples
#' \dontrun{
#' # This example is not executed since it requires a network connection
#' # to ArcGIS Online and a valid authentication token
#' library(sf)
#' library(arcgisutils)
#' set_arc_token(auth_user())
#'
#' facility <- st_sfc(st_point(c(-122.253, 37.757)), crs = 4326)
#' find_service_areas(facility)
#' }
#'
#' @family direct
#' @family service area
#' @export
#' @references [API Reference](https://developers.arcgis.com/rest/routing/serviceArea-service-direct/)
find_service_areas <- function(
  facilities,
  default_breaks = c(5, 10, 15),
  travel_mode = NULL,
  travel_direction = "away",
  time_of_day = NULL,
  output_polygons = "simplified",
  split_polygons_at_breaks = TRUE,
  overlap_polygons = TRUE,
  merge_similar_polygon_ranges = FALSE,
  trim_outer_polygons = TRUE,
  output_lines = NULL,
  split_lines_at_breaks = TRUE,
  overlap_lines = TRUE,
  return_geometry = "service_areas",
  u_turns = NULL,
  use_hierarchy = NULL,
  impedance = NULL,
  accumulate_impedance = NULL,
  restrictions = NULL,
  trim_polygon_distance = 100,
  trim_polygon_distance_units = "meters",
  point_barriers = NULL,
  line_barriers = NULL,
  polygon_barriers = NULL,
  ignore_invalid_locations = TRUE,
  output_geometry_precision = 10,
  output_geometry_precision_units = "meters",
  crs = 4326,
  token = arcgisutils::arc_token()
) {
  obj_check_token(token)

  if (vctrs::vec_size(facilities) > 100) {
    cli::cli_abort("The maximum number of facilities is 100.")
  }

  if (!is.null(point_barriers) && vctrs::vec_size(point_barriers) > 250) {
    cli::cli_abort("The maximum number of point barriers is 250.")
  }

  check_bool(split_polygons_at_breaks)
  check_bool(overlap_polygons)
  check_bool(merge_similar_polygon_ranges)
  check_bool(trim_outer_polygons)
  check_number_whole(trim_polygon_distance)
  check_bool(use_hierarchy, allow_null = TRUE)
  check_bool(split_lines_at_breaks)
  check_bool(overlap_lines)
  check_bool(ignore_invalid_locations)
  check_number_decimal(output_geometry_precision)

  facilities <- as_stops(facilities)
  time_of_day <- validate_time_of_day(time_of_day)
  time_of_day_is_utc <- if (!is.null(time_of_day)) {
    is_utc(time_of_day)
  } else {
    FALSE
  }

  travel_direction <- validate_travel_dir(travel_direction)
  u_turns <- validate_u_turns(u_turns)
  impedance <- validate_impedance_value(impedance)
  accumulate_impedance <- validate_impedance_value(
    accumulate_impedance,
    multiple = TRUE
  )
  restrictions <- validate_restrictions(restrictions)
  travel_mode <- validate_travel_mode(travel_mode, token = token)

  output_polygons <- validate_output_polygons(output_polygons)
  output_lines <- validate_output_lines(output_lines)
  trim_polygon_distance_units <- validate_distance_units_esri(
    trim_polygon_distance_units
  )
  output_geometry_precision_units <- validate_distance_units_esri(
    output_geometry_precision_units
  )
  return_geometry <- validate_return_geometry_sa(return_geometry)
  point_barriers <- as_point_barriers(point_barriers)
  line_barriers <- as_polyline_barriers(line_barriers)
  polygon_barriers <- as_polygon_barriers(polygon_barriers)

  params <- compact(list(
    facilities = facilities,
    travelMode = travel_mode,
    defaultBreaks = paste(default_breaks, collapse = ","),
    travelDirection = travel_direction,
    timeOfDay = time_of_day,
    timeOfDayIsUTC = time_of_day_is_utc,
    outputPolygons = output_polygons,
    splitPolygonsAtBreaks = split_polygons_at_breaks,
    overlapPolygons = overlap_polygons,
    mergeSimilarPolygonRanges = merge_similar_polygon_ranges,
    trimOuterPolygons = trim_outer_polygons,
    trimPolygonDistance = trim_polygon_distance,
    trimPolygonDistanceUnits = trim_polygon_distance_units,
    restrictUTurns = u_turns,
    useHierarchy = use_hierarchy,
    impedanceAttributeName = impedance,
    accumulateAttributeNames = accumulate_impedance,
    restrictionAttributeNames = restrictions,
    barriers = point_barriers,
    polylineBarriers = line_barriers,
    polygonBarriers = polygon_barriers,
    splitLinesAtBreaks = split_lines_at_breaks,
    outputLines = output_lines,
    overlapLines = overlap_lines,
    returnPolygons = return_geometry$returnPolygons,
    returnPolylines = return_geometry$returnPolylines,
    returnFacilities = return_geometry$returnFacilities,
    returnBarriers = return_geometry$returnBarriers,
    returnPolylineBarriers = return_geometry$returnPolylineBarriers,
    returnPolygonBarriers = return_geometry$returnPolygonBarriers,
    ignoreInvalidLocations = ignore_invalid_locations,
    outputGeometryPrecision = output_geometry_precision,
    outputGeometryPrecisionUnits = output_geometry_precision_units,
    outSR = arcgisutils::as_spatial_reference(crs),
    f = "json"
  ))

  meta <- detect_errors(arcgisutils::arc_portal_self(token))
  service_url <- meta$helperServices$serviceArea$url

  if (is.null(service_url)) {
    cli::cli_abort("Cannot find service area URL for this token")
  }

  resp <- arcgisutils::arc_base_req(
    service_url,
    token,
    path = "solveServiceArea",
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
    service_areas = try_parse(resp, query = "/saPolygons"),
    sa_lines = try_parse(resp, query = "/saPolylines"),
    facilities = try_parse(resp, query = "/facilities"),
    barriers = try_parse(resp, query = "/barriers"),
    polyline_barriers = try_parse(resp, query = "/polylineBarriers"),
    polygon_barriers = try_parse(resp, query = "/polygonBarriers"),
    messages = RcppSimdJson::fparse(resp, query = "/messages")
  ))
}

validate_travel_dir <- function(
  x,
  error_arg = rlang::caller_arg(x),
  error_call = rlang::caller_call()
) {
  check_string(
    x,
    allow_null = TRUE,
    arg = error_arg,
    call = error_call
  )

  if (is.null(x)) {
    return(NULL)
  }

  x <- rlang::arg_match0(
    x,
    values = c("away", "towards"),
    arg_nm = error_arg,
    error_call = error_call
  )

  lu <- c(
    "away" = "esriNATravelDirectionFromFacility",
    "towards" = "esriNATravelDirectionToFacility"
  )

  unname(lu[x])
}

validate_output_lines <- function(
  x,
  error_arg = rlang::caller_arg(x),
  error_call = rlang::caller_call()
) {
  check_string(
    x,
    allow_null = TRUE,
    arg = error_arg,
    call = error_call
  )

  if (is.null(x)) {
    return("esriNAOutputLineNone")
  }

  x <- rlang::arg_match0(
    x,
    values = c("true_shape", "with_measure"),
    arg_nm = error_arg,
    error_call = error_call
  )

  lu <- c(
    "true_shape" = "esriNAOutputLineTrueShape",
    "with_measure" = "esriNAOutputLineTrueShapeWithMeasure"
  )

  unname(lu[x])
}

validate_return_geometry_sa <- function(
  x,
  error_arg = rlang::caller_arg(x),
  error_call = rlang::caller_env()
) {
  check_character(x, arg = error_arg, call = error_call)

  return_geometry_lu <- c(
    "service_areas" = "returnPolygons",
    "sa_lines" = "returnPolylines",
    "facilities" = "returnFacilities",
    "barriers" = "returnBarriers",
    "polyline_barriers" = "returnPolylineBarriers",
    "polygon_barriers" = "returnPolygonBarriers"
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

validate_output_polygons <- function(
  x,
  error_arg = rlang::caller_arg(x),
  error_call = rlang::caller_call()
) {
  check_string(x, allow_null = TRUE)
  if (is.null(x)) {
    return("esriNAOutputPolygonNone")
  }

  check_string(x, arg = error_arg, call = error_call)

  x <- rlang::arg_match0(
    x,
    values = c("detailed", "simplified"),
    arg_nm = error_arg,
    error_call = error_call
  )

  lu <- c(
    "detailed" = "esriNAOutputPolygonDetailed",
    "simplified" = "esriNAOutputPolygonSimplified"
  )

  unname(lu[x])
}

validate_distance_units_esri <- function(
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
      "meters",
      "kilometers",
      "feet",
      "miles",
      "nautical_miles",
      "yards"
    ),
    arg_nm = error_arg,
    error_call = error_call
  )

  lu <- c(
    "meters" = "esriMeters",
    "kilometers" = "esriKilometers",
    "feet" = "esriFeet",
    "miles" = "esriMiles",
    "nautical_miles" = "esriNauticalMiles",
    "yards" = "esriYards"
  )

  unname(lu[x])
}
