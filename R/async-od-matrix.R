travel_cost_matrix_async <- function(
  origins,
  destinations = NULL,
  travel_mode = NULL,
  time_units = NULL,
  distance_units = NULL,
  analysis_region = NULL,
  n_dests = NULL,
  cutoff = NULL, 
  time_of_day = NULL,
  use_hierarchy = NULL,
  u_turns = NULL,
  restrictions = NULL,
  impedance = NULL,
  time_impedance = NULL,
  distance_impedance = NULL,
  point_barriers = NULL,
  line_barriers = NULL,
  polygon_barriers = NULL,
  token = arcgisutils::arc_token()
  # attribute_parameter_values: TODO (unimplemented)
  # save_output_network_analysis_layer = FALSE
) {

  time_units <- validate_time_units(time_units)

  # validate travel_mode
  if (!is.null(travel_mode)) {
    check_string(travel_mode)
    available_modes <- retrieve_travel_modes(token)[["supportedTravelModes"]]
    mode_idx <- which(available_modes[["id"]] == travel_mode)
    if (length(mode_idx) == 0) {
      cli::cli_abort(
        c(
          "{.arg travel_mode} ID is not found.",
          "i" = "use {.fn retrieve_travel_modes} to identify available travel modes."
        )
      )
    }

    mode_attrs <- available_modes[["attributeParameterValues"]][[mode_idx]]
    # convert to a json string
    travel_mode <- yyjsonr::write_json_str(unclass(mode_attrs), auto_unbox = TRUE)
  }

  distance_units <- validate_distance_units(distance_units)
  analysis_region <- validate_analysis_region(analysis_region)
  check_number_whole(n_dests, allow_null = TRUE, min = 0)
  check_number_decimal(cutoff, min = 0, allow_null = TRUE)
  time_of_day <- validate_time_of_day(time_of_day)
  
  # set time of day is UTC parameter
  if (!is.null(time_of_day)) {
    .utc <- is_utc(time_of_day)
    time_zone_for_time_of_day <- ifelse(.utc, "UTC", "Geographically Local")
  } else {
    time_zone_for_time_of_day <- NULL
  }

  u_turns <- validate_u_turns_async(u_turns)
  check_bool(use_hierarchy, allow_null = TRUE)
  restrictions <- validate_restrictions(restrictions)
  impedance <- validate_impedance_value(impedance)
  time_impedance <- validate_time_impedance(time_impedance)
  distance_impedance <- validate_distance_impedance(distance_impedance)

  params <- list(
      origins = as_od_points(origins),
      destinations = as_od_points(destinations),
      travel_mode = travel_mode,
      time_units = time_units,
      distance_units = distance_units,
      analysis_region = analysis_region,
      number_of_destinations_to_find = n_dests,
      cutoff = cutoff,
      time_of_day = time_of_day,
      time_zone_for_time_of_Day = time_zone_for_time_of_day,
      point_barriers = as_point_barriers(point_barriers),
      line_barriers = as_polyline_barriers(line_barriers),
      polygon_barriers = as_polygon_barriers(polygon_barriers),
      uturn_at_junctions = u_turns,
      use_hierarchy = use_hierarchy,
      restrictions = restrictions,
      # attribute_parameter_values 
      # origin_destination_line_shape
      # save_output_network_analysis_layer
      # overrides
      impedance = impedance,
      time_impedance = time_impedance,
      distance_impedance = distance_impedance,
      output_format = "CSV File",
      f = "json"
      # ignore_invalid_locations
      # locate_settings
  )

  # get service url 
  meta <- arcgisutils::arc_self_meta(token)


  burl <- httr2::req_url_path_append(
    httr2::request(meta$helperServices$asyncODCostMatrix$url),
    "GenerateOriginDestinationCostMatrix"
  )$url
  
  new_esri_job$new(burl, params, token)

}

validate_time_units <- function(
    x,
    error_arg = rlang::caller_arg(x),
    error_call = rlang::caller_call()
) {

  if (is.null(x)) return(NULL)

  # time unit look up
  tu_lu <- c(
    "seconds" = "Seconds",
    "minutes" = "Minutes",
    "hours" = "Hours",
    "days" = "Days"
  )

  # validate the provided value
  x <- rlang::arg_match(
    x,
    error_arg = error_arg,
    error_call = error_call
  )

  tu_lu[x]
}

validate_distance_units <- function(
    distance_units,
    error_arg = rlang::caller_arg(distance_units),
    error_call = rlang::caller_call()
) {

  if (is.null(distance_units)) return(NULL)
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

  lu[x]

}

validate_analysis_region <- function(
    analysis_region,
    error_arg = rlang::caller_arg(analysis_region),
    error_call = rlang::caller_call()
) {

  if (is.null(analysis_region)) return(NULL)
  lu <- setNames(
    c("Europe", "Japan", "Korea", "MiddleEastAndAfrica", "NorthAmerica", "SouthAmerica", "SouthAsia", "Thailand"),
    c("europe", "japan", "korea", "middle_east_and_africa", "north_america", "south_america", "south_asia", "thailand")
  )

  x <- rlang::arg_match(
    analysis_region,
    names(lu),
    error_arg = error_arg,
    error_call = error_call
  )

  lu[x]
}

validate_u_turns_async <- function(
    u_turns,
    error_arg = rlang::caller_arg(analysis_region),
    error_call = rlang::caller_call()
) {
  if (is.null(u_turns)) return(NULL)
  lu <- setNames(
    c("Allowed", "Allowed only at Intersections and Dead Ends", "Allowed only at Dead Ends", "Not Allowed"),
    c("allow_backtrack", "deadend_intersection", "deadend", "no_backtrack")
  )

  x <- rlang::arg_match(
    u_turns,
    names(lu),
    error_arg = error_arg,
    error_call = error_call
  )

  lu[x]
}

validate_time_impedance <- function(
    time_impedance,
    error_arg = rlang::caller_arg(time_impedance),
    error_call = rlang::caller_call()
) { 

  # Note the docs say 
  # "These value are specific to the services published with the ArcGIS StreetMap Premium data. The values will be different if you are using other data for the analysis."
  # so this might be incorrect for other network analysis environment
  if (is.null(time_impedance)) return(NULL)
  
  lu <- setNames(
    c("Minutes", "TravelTime", "WalkTime", "TruckMinutes", "TruckTravelTime"),
    c("minutes", "travel_time", "walk_time", "truck_minutes", "truck_travel_time")
  )

  x <- rlang::arg_match(
    time_impedance,
    names(lu),
    error_arg = error_arg,
    error_call = error_call
  )

  lu[x]

}

validate_distance_impedance <- function(
  distance_impedance,
  error_arg = rlang::caller_arg(distance_impedance),
  error_call = rlang::caller_call()
) { 

  # Note the docs say 
  # "These value are specific to the services published with the ArcGIS StreetMap Premium data. The values will be different if you are using other data for the analysis."
  # so this might be incorrect for other network analysis environment
  if (is.null(distance_impedance)) return(NULL)

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

  lu[x]

}