#' Travel Cost Matrix
#'
#' Creates an origin-destination cost matrix containing the travel cost between every origin and destination.
#'
#' @param origins an `sf` or `sfc` object containing point geometries representing the starting points.
#' @param destinations default `origins`. An `sf` or `sfc` object containing point geometries representing the ending points.
#' @param default_target_destination_count default `NULL`. An integer scalar. The maximum number of destinations to find per origin.
#' @param output_type default `NULL`. A scalar character. One of `"no_lines"`, `"straight_lines"`, `"true_shape"`. Controls whether route geometry is returned.
#' @param return_geometry default `character(0)`. A character vector. Valid values: `"origins"`, `"destinations"`, `"barriers"`, `"polyline_barriers"`, `"polygon_barriers"`.
#' @inheritParams find_closest_facility
#' @inheritParams find_service_areas
#'
#' @returns A named list. Elements present depend on `return_geometry` and `output_type`:
#' - `od_lines`: OD cost matrix features
#' - `origins`: origin features
#' - `destinations`: destination features
#' - `barriers`: point barrier features
#' - `polyline_barriers`: polyline barrier features
#' - `polygon_barriers`: polygon barrier features
#' - `messages`: status and warning messages from the service
#'
#' @examples
#' \dontrun{
#' library(sf)
#'
#' origins <- st_sfc(
#'   st_point(c(-122.4194, 37.7749)),
#'   st_point(c(-122.4313, 37.7793)),
#'   crs = 4326
#' )
#'
#' destinations <- st_sfc(
#'   st_point(c(-122.4083, 37.7858)),
#'   st_point(c(-122.4000, 37.7900)),
#'   st_point(c(-122.4561, 37.7513)),
#'   crs = 4326
#' )
#'
#' result <- travel_cost_matrix(
#'   origins = origins,
#'   destinations = destinations
#' )
#'
#' result
#' }
#'
#' @export
#' @references [API Reference](https://developers.arcgis.com/rest/routing/od-cost-matrix-synchronous-service/)
travel_cost_matrix <- function(
  origins,
  destinations = origins,
  travel_mode = NULL,
  default_cutoff = NULL,
  default_target_destination_count = NULL,
  output_type = NULL,
  time_of_day = NULL,
  u_turns = NULL,
  use_hierarchy = NULL,
  impedance = NULL,
  accumulate_impedance = NULL,
  restrictions = NULL,
  attribute_parameter_values = NULL,
  point_barriers = NULL,
  line_barriers = NULL,
  polygon_barriers = NULL,
  return_geometry = character(0),
  ignore_invalid_locations = TRUE,
  return_empty_results = FALSE,
  geometry_precision = NULL,
  locate_settings = NULL,
  crs = 4326,
  token = arcgisutils::arc_token()
) {
  obj_check_token(token)

  check_bool(ignore_invalid_locations)
  check_bool(return_empty_results)
  check_bool(use_hierarchy, allow_null = TRUE)
  check_number_decimal(default_cutoff, allow_null = TRUE)
  check_number_whole(default_target_destination_count, allow_null = TRUE)
  check_string(geometry_precision, allow_null = TRUE)

  origins <- as_stops(origins)
  destinations <- as_stops(destinations)

  time_of_day <- validate_time_of_day(time_of_day)
  time_of_day_is_utc <- if (!is.null(time_of_day)) {
    is_utc(time_of_day)
  } else {
    FALSE
  }

  u_turns <- validate_u_turns(u_turns)
  impedance <- validate_impedance_value(impedance)
  accumulate_impedance <- validate_impedance_value(
    accumulate_impedance,
    multiple = TRUE
  )
  restrictions <- validate_restrictions(restrictions)
  travel_mode <- validate_travel_mode(travel_mode, token = token)
  output_type <- validate_od_output_type(output_type)
  return_geometry <- validate_return_geometry_od(return_geometry)

  point_barriers <- as_point_barriers(point_barriers)
  line_barriers <- as_polyline_barriers(line_barriers)
  polygon_barriers <- as_polygon_barriers(polygon_barriers)

  params <- compact(list(
    origins = origins,
    destinations = destinations,
    travelMode = travel_mode,
    defaultCutoff = default_cutoff,
    defaultTargetDestinationCount = default_target_destination_count,
    outputType = output_type,
    timeOfDay = time_of_day,
    timeOfDayIsUTC = time_of_day_is_utc,
    restrictUTurns = u_turns,
    useHierarchy = use_hierarchy,
    impedanceAttributeName = impedance,
    accumulateAttributeNames = accumulate_impedance,
    restrictionAttributeNames = restrictions,
    attributeParameterValues = attribute_parameter_values,
    barriers = point_barriers,
    polylineBarriers = line_barriers,
    polygonBarriers = polygon_barriers,
    returnOrigins = return_geometry$returnOrigins,
    returnDestinations = return_geometry$returnDestinations,
    returnBarriers = return_geometry$returnBarriers,
    returnPolylineBarriers = return_geometry$returnPolylineBarriers,
    returnPolygonBarriers = return_geometry$returnPolygonBarriers,
    ignoreInvalidLocations = ignore_invalid_locations,
    returnEmptyResults = return_empty_results,
    geometryPrecision = geometry_precision,
    locateSettings = locate_settings,
    outSR = arcgisutils::as_spatial_reference(crs),
    f = "json"
  ))

  meta <- detect_errors(arcgisutils::arc_portal_self(token))
  service_url <- meta$helperServices$odCostMatrix$url

  if (is.null(service_url)) {
    cli::cli_abort("Cannot find OD cost matrix URL for this token")
  }

  resp <- arcgisutils::arc_base_req(
    service_url,
    token,
    path = "solveODCostMatrix",
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
    od_lines = try_parse(resp, query = "/odLines"),
    origins = try_parse(resp, query = "/origins"),
    destinations = try_parse(resp, query = "/destinations"),
    barriers = try_parse(resp, query = "/barriers"),
    polyline_barriers = try_parse(resp, query = "/polylineBarriers"),
    polygon_barriers = try_parse(resp, query = "/polygonBarriers"),
    messages = RcppSimdJson::fparse(resp, query = "/messages")
  ))
}

validate_od_output_type <- function(
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
    values = c("no_lines", "straight_lines", "true_shape"),
    arg_nm = error_arg,
    error_call = error_call
  )

  lu <- c(
    "no_lines" = "esriNAODOutputNoLines",
    "straight_lines" = "esriNAODOutputStraightLines",
    "true_shape" = "esriNAODOutputTrueShapeLines"
  )

  unname(lu[x])
}

validate_return_geometry_od <- function(
  x,
  error_arg = rlang::caller_arg(x),
  error_call = rlang::caller_env()
) {
  check_character(x, arg = error_arg, call = error_call)

  x <- rlang::arg_match(
    x,
    values = c(
      "origins",
      "destinations",
      "barriers",
      "polyline_barriers",
      "polygon_barriers"
    ),
    multiple = TRUE,
    error_arg = error_arg,
    error_call = error_call
  )

  return_geometry_lu <- c(
    "origins" = "returnOrigins",
    "destinations" = "returnDestinations",
    "barriers" = "returnBarriers",
    "polyline_barriers" = "returnPolylineBarriers",
    "polygon_barriers" = "returnPolygonBarriers"
  )

  api_params <- return_geometry_lu[x]
  as.list(
    setNames(rep(TRUE, length(api_params)), api_params)
  )
}


#' @keywords internal
#' @noRd
validate_time_of_day <- function(time_of_day) {
  if (is.null(time_of_day)) {
    return(NULL)
  }
  if (inherits(time_of_day, "character")) {
    check_string(time_of_day)
    caller <- rlang::caller_call()
    time_of_day <- tryCatch(
      as.POSIXlt(time_of_day),
      error = function(e) {
        cli::cli_abort(
          c(
            "Failed to parse {.arg time_of_day} as a date time object",
            ">" = e[["message"]]
          ),
          call = caller
        )
      }
    )
  }

  if (inherits(time_of_day, "POSIXt")) {
    if (length(time_of_day) != 1) {
      cli::cli_abort("{.arg time_of_day} must be a scalar")
    }
    time_of_day <- as.POSIXlt(time_of_day)
  } else if (!is.null(time_of_day)) {
    cli::cli_abort("{.arg time_of_day} must be a character or POSIX scalar")
  }
  arcgisutils::date_to_ms(time_of_day)
}


is_utc <- function(x, arg = rlang::caller_arg(x)) {
  if (!arcgisutils::is_date(x)) {
    cli::cli_abort("{.arg {arg}} is not a date or datetime")
  }
  "UTC" %in% attr(as.POSIXlt(x), "tzone")
}


validate_u_turns <- function(
  x,
  error_arg = rlang::caller_arg(),
  error_call = rlang::caller_call()
) {
  if (is.null(x)) {
    return(x)
  }
  x <- rlang::arg_match(
    x,
    values = c(
      "allow_backtrack",
      "deadend_intersection",
      "deadend",
      "no_backtrack"
    ),
    error_call = error_call,
    error_arg = error_arg
  )

  lu <- setNames(
    c(
      "esriNFSBAllowBacktrack",
      "esriNFSBAtDeadEndsAndIntersections",
      "esriNFSBAtDeadEndsOnly",
      "esriNFSBNoBacktrack"
    ),
    c("allow_backtrack", "deadend_intersection", "deadend", "no_backtrack")
  )

  unname(lu[x])
}


validate_restrictions <- function(
  x,
  error_arg = rlang::caller_arg(x),
  error_call = rlang::caller_call()
) {
  # early return for NULL
  if (is.null(x)) {
    return(x)
  }

  # known restriction types
  restrictions <- c(
    "Any Hazmat Prohibited",
    "Avoid Carpool Roads",
    "Avoid Express Lanes",
    "Avoid Gates",
    "Avoid Limited Access Roads",
    "Avoid Private Roads",
    "Avoid Roads Unsuitable for Pedestrians",
    "Avoid Stairways",
    "Avoid Toll Roads",
    "Avoid Toll Roads for Trucks",
    "Avoid Truck Restricted Roads",
    "Avoid Unpaved Roads",
    "Axle Count Restriction",
    "Driving a Bus",
    "Driving a Taxi",
    "Driving a Truck",
    "Driving an Automobile",
    "Driving an Emergency Vehicle",
    "Height Restriction",
    "Kingpin to Rear Axle Length Restriction",
    "Length Restriction",
    "Preferred for Pedestrians",
    "Riding a Motorcycle",
    "Roads Under Construction Prohibited",
    "Semi or Tractor with One or More Trailers Prohibited",
    "Single Axle Vehicles Prohibited",
    "Tandem Axle Vehicles Prohibited",
    "Through Traffic Prohibited",
    "Truck with Trailers Restriction",
    "Use Preferred Hazmat Routes",
    "Use Preferred Truck Routes",
    "Walking",
    "Weight Restriction",
    "Weight per Axle Restriction",
    "Width Restriction"
  )

  # set to lowercase for standardized comparision
  restrictions_lower <- tolower(restrictions)

  # create a lookup vector because we need caps
  lu <- setNames(restrictions, restrictions_lower)

  # set to lowercase for check
  x <- tolower(x)

  # check the provided args
  restrictions <- rlang::arg_match(
    x,
    restrictions_lower,
    multiple = TRUE,
    error_arg = error_arg,
    error_call = error_call
  )

  # if there is more than one result we need to collapse
  if (length(restrictions) > 1) {
    paste(lu[restrictions], collapse = ",")
  } else {
    unname(lu[restrictions])
  }
}


#' @export
st_count <- function(x) {
  UseMethod("st_count")
}

#' @export
st_count.sf <- function(x) nrow(x)
#' @export
st_count.sfc <- function(x) length(x)
