service_area_job <- R6::R6Class("service_area_job", inherit = esri_job)

# TODO 
# set output format / output type
#' @export
service_areas_async <- function(
  facilities,
  travel_mode = NULL,  # 
  break_values = NULL,
  break_units = NULL,
  travel_direction = c("away", "towards"), # 
  time_of_day = NULL, # 
  use_hierarchy = NULL, #
  u_turns = NULL, # 
  polygons_for_multiple_facilities = c("overlapping", "not overlapping", "merge"), # RENAME  # 
  polygon_overlap_type = c("rings", "disks"), # RENAME # 
  polygon_trim_distance = NULL, # 
  polygon_simplification_tolerance = NULL, # 
  polygon_detail = NULL, # 
  impedance = NULL, # 
  time_impedance = NULL, # 
  distance_impedance = NULL, # 
  analysis_region = NULL, # 
  restrictions = NULL, # 
  point_barriers = NULL, # 
  line_barriers = NULL,# 
  polygon_barriers = NULL,#  
  token = arcgisutils::arc_token()
) {

  check_bool(use_hierarchy, allow_null = TRUE)
  travel_mode <- validate_travel_mode(travel_mode, token = token)
  break_units <- validate_break_units(break_units)
  break_values <- validate_break_values(break_values)
  travel_direction <- validate_travel_direction(travel_direction)
  analysis_region <- validate_analysis_region(analysis_region)
  u_turns <- validate_u_turns_async(u_turns)
  restrictions <- validate_restrictions(restrictions)
  impedance <- validate_impedance_value(impedance)
  time_impedance <- validate_time_impedance(time_impedance)
  distance_impedance <- validate_distance_impedance(distance_impedance)
  time_of_day <- validate_time_of_day(time_of_day)
  time_zone_for_time_of_day <- validate_tz_for_time_of_day(time_of_day)
  polygons_for_multiple_facilities <-  validate_multiple_facilities(polygons_for_multiple_facilities)
  polygon_overlap_type <- validate_overlap_type(polygon_overlap_type)
  polygon_trim_distance <- validate_tolerance(polygon_trim_distance)
  polygon_simplification_tolerance <- validate_tolerance(polygon_simplification_tolerance)
  polygon_detail <- validate_detail(polygon_detail)

  point_barriers <- as_point_barriers(point_barriers)
  line_barriers <- as_polyline_barriers(line_barriers)
  polygon_barriers <- as_polygon_barriers(polygon_barriers)

  params <- list(
    facilities = as_od_points(facilities),
    travel_mode = travel_mode,
    break_values = break_values,
    break_units = break_units,
    travel_direction = travel_direction,
    time_of_day = time_of_day,
    time_zone_for_time_of_day = time_zone_for_time_of_day,
    use_hierarchy = use_hierarchy,
    uturn_at_junctions = u_turns,
    polygons_for_multiple_facilities = polygons_for_multiple_facilities,
    polygon_overlap_type = polygon_overlap_type,
    polygon_trim_distance = polygon_trim_distance,
    polygon_simplification_tolerance = polygon_simplification_tolerance,
    point_barriers = point_barriers,
    line_barriers = line_barriers,
    polygon_barriers = polygon_barriers,
    restrictions = restrictions,
    impedance = impedance,
    analysis_region = analysis_region,
    time_impedance = time_impedance,
    distance_impedance = distance_impedance,
    polygon_detail = polygon_detail,
    output_type = "Polygons",
    output_format = "JSON File",
    f = "json"
  )

    # get service url 
    meta <- arcgisutils::arc_self_meta(token)
    
    service_area_job$new(meta$helperServices$asyncServiceArea$url, params, token)
  
}

validate_travel_direction <- function(x, error_arg = rlang::caller_arg(x), error_call = rlang::caller_call()) {
  if (is.null(x)) return(NULL)
  x <- rlang::arg_match(
    x,
    c("away", "towards"),
    error_arg = error_arg,
    error_call = error_call
  )

  lu <- setNames(
    c("Away From Facility", "Towards Facility"),
    c("away", "towards")
  )

  unname(lu[x])
}

validate_tz_for_time_of_day <- function(time_of_day) {
  if (is.null(time_of_day)) {
    return(NULL)
  }

  .utc <- is_utc(time_of_day)
  ifelse(.utc, "UTC", "Geographically Local")

}

validate_multiple_facilities <- function(x, error_arg = rlang::caller_arg(x), error_call = rlang::caller_call()) {
  if (is.null(x)) return(NULL)
    x <- rlang::arg_match(
      x,
      c("overlapping", "not overlapping", "merge"),
      error_arg = error_arg,
      error_call = error_call
    )
  
    lu <- setNames(
      c("Overlapping", "Not Overlapping", "Merge by Break Value"),
      c("overlapping", "not overlapping", "merge")
    )
  
    unname(lu[x])
}

validate_overlap_type <- function(
    x,
    error_arg = rlang::caller_arg(x),
    error_call = rlang::caller_call()
) {
  if (is.null(x)) return(NULL)
    x <- rlang::arg_match(
      x,
      c("rings", "disks"),
      error_arg = error_arg,
      error_call = error_call
    )
  
    lu <- setNames(
      c("Rings", "Disks"),
      c("rings", "disks")
    )
  
    unname(lu[x])
}


validate_units <- function(
    units,
    error_arg = rlang::caller_arg(units),
    error_call = rlang::caller_call()
) {
  
  esri_units <- c("esriCentimeters", "esriDecimalDegrees", "esriDecimeters", "esriFeet", "esriInches", "esriKilometers", "esriMeters", "esriMiles", "esriMillimeters", "esriNauticalMiles", "esriPoints", "esriYards")

  unit_names <- c("centimeters", "decimaldegrees", "decimeters", "feet", "inches", 
  "kilometers", "meters", "miles", "millimeters", "nautical_miles", 
  "points", "yards")

  lu <- setNames(esri_units, unit_names)

  x <- rlang::arg_match(
    units,
    unit_names,
    error_arg = error_arg,
    error_call = error_call
  )

  unname(lu[x])
}


validate_tolerance <- function(
    x,
    error_arg = rlang::caller_arg(x),
    error_call = rlang::caller_call()
) {
  if (is.null(x)) {
    return(NULL)
  }

  if (!rlang::is_list(x, 2L)) {
    cli::cli_abort(
      c(
        "Expected list with two elements {.val distance} and {.val units}",
        "Found {obj_type_friendly(x)}"
      ),
      call = error_call
    )
  }
  # check that distance is a number 
  check_number_decimal(x[["distance"]], min = 0, call = error_call, arg = error_arg)
  x[["units"]] <- validate_units(x[["units"]], error_arg = error_arg, error_call = error_call)

  yyjsonr::write_json_str(x, auto_unbox = TRUE)
}



validate_detail <- function(
    x,
    error_arg = rlang::caller_arg(x),
    error_call = rlang::caller_call()
) {
  if (is.null(x)) {
    return(NULL)
  }

  opts <- c("standard", "generalized", "high")
  opt_names <- c("Standard", "Generalized", "High")

  x <- rlang::arg_match(
    x,
    opts,
    error_arg = error_arg,
    error_call = error_call
  )

  lu <- setNames(opt_names, opts)
  unname(lu[x])
}


validate_break_units <- function(
    x,
    error_arg = rlang::caller_arg(x),
    error_call = rlang::caller_call()
) {
  if (is.null(x)) {
  return(NULL)
  }

  lu <- c(
    "meters" = "Meters",
    "kilometers" = "Kilometers",
    "feet" = "Feet",
    "yards" = "Yards",
    "miles" = "Miles",
    "nautical_miles" = "NauticalMiles",
    "seconds" = "Seconds",
    "minutes" = "Minutes",
    "hours" = "Hours",
    "days" = "Days"
  )

  x <- rlang::arg_match(
    distance_units,
    names(lu),
    error_arg = error_arg,
    error_call = error_call
  )

  unname(lu[x])
}

validate_break_values <- function(
    x,
    error_arg = rlang::caller_arg(x),
    error_call = rlang::caller_call()
) {
  if (is.null(x)) {
    return(NULL)
  }

  if (!rlang::is_bare_numeric(x)) {
    cli::cli_abort(
      c(
        "{.arg {error_arg}} must be a numeric vector.",
        "i" = "Found {obj_type_friendly(x)}."
      )
    )
  }
  paste(x, collapse = " ")
}
