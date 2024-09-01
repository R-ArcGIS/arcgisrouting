# Synchronous service
# 
# https://developers.arcgis.com/rest/routing/origin-destination-cost-matrix-synchronous-service/#origins
# 
# Required parameters
#   origins - points comma separated or feature collection
#   destinations - same as above
#   token - 
#   f - json
# Optional parameters
#   travelMode - m
#   defaultCutoff - numeric
#   defaultTargetDestinationCount - 
#   outputType - I think this can be omitted
#   timeOfDay - default uses the local time 
#   timeOfDayIsUTC - automatically set this is time of day is provided
#   useHierarchy - easy
#   restrictUTurns - 
#   impedanceAttributeName
#   accumulateAttributeNames
#   restrictionAttributeNames
#   attributeParameterValues
#   barriers
#   polylineBarriers
#   polygonBarriers
#   returnOrigins
#   returnDestinations
#   returnBarriers


#' @param travel_mode default `NULL`. A scalar character of the travel mode's ID. 
#' @param cutoff default `NULL`. Determines the maximum distance or time to stop searching.
#' @param n_target_destinations a numeric scalar. By default all origins and destinations are matched.
#' @param time_of_day default `NULL`. A character or `POSIXlt` scalar representing the time of day when the cost is to be calculated. If a character vector is provided, it as processed with `as.POSIXlt()` and uses the default time zone. If the time zone is `UTC` the parameter [`timeOfDayIsUTC`](https://developers.arcgis.com/rest/routing/origin-destination-cost-matrix-synchronous-service/#timeofdayisutc) is set to `true`. 
#' @param use_hierarchy default `NULL`. Prefer higher-order streets (e.g. freeways). Supressed by `travel_mode`. ([Reference](https://developers.arcgis.com/rest/routing/origin-destination-cost-matrix-synchronous-service/#usehierarchy))
#' @param u_turns default `"allow_backtrack"`. Must be one of `"allow_backtrack"`, `"deadend_intersection"`, `"deadend"`, or `"no_backtrack"`. Determines the conditions when a U-turn is permitted. 
#' 
#' ## Argument Details
#' 
#' ### `travel_mode`
#' 
#' When the `travel_mode` parameter is set, you are choosing a travel mode configured in your organization, and the service automatically overrides the values of other parameters with values that model the chosen travel mode. The following parameters are overridden: `impedance_attribute_name` , `attribute_parameter_values` , `restrict_uturns` , `use_hierarchy` , `restriction_attribute_names` , and `directions_time_attribute_name`. ([Reference](https://developers.arcgis.com/rest/routing/origin-destination-cost-matrix-synchronous-service/#travelmode))
#' 
#' ### `cutoff`
#' 
#' The travel time or travel distance value at which to stop searching for destinations from a given origin. ([Reference](https://developers.arcgis.com/rest/routing/origin-destination-cost-matrix-synchronous-service/#defaultcutoff))
#' 
#' ### `n_target_destinations`
#' 
#' The maximum number of destinations to find per origin. If a value for this parameter is not specified, the output matrix includes travel costs from each origin to every destination. ([Reference](https://developers.arcgis.com/rest/routing/origin-destination-cost-matrix-synchronous-service/#defaulttargetdestinationcount))


#' @export
travel_cost_matrix <- function(
  origin,
  destinations,
  travel_mode = NULL,
  cutoff = NULL, # defaultCutoff
  time_of_day = NULL,
  use_hierarchy = NULL,
  u_turns = NULL, 

) {
  # travel_mode, if provided needs to be turned into JSON from 
  # retrieve_travel_modes() stored in the attributeParameterValues column

  # TODO default_cutoff: we can set Cutoff_[Impedance] to provide a per feature
  # cutoff value. How do we set this???

  # if time_of_day is a character try and parse it
  check_time_of_day(time_of_day)

  # set time of day is UTC parameter
  if (!is.null(time_of_day)) {
    time_of_day_is_utc <- is_utc(time_of_day) 
  } else {
    time_of_day_is_utc <- FALSE
  }

  check_bool(use_hierarchy, allow_null = TRUE)

  # validate the u-turnsargument
  u_turns <- validate_u_turns(u_turns)
}


#' @keywords internal
#' @noRd
check_time_of_day <- function(time_of_day) {
  if (inherits(time_of_day, "character")) {
    check_string(time_of_day)
    caller <- rlang::caller_call() 
    time_of_day <- tryCatch(
      as.POSIXlt(time_of_day), 
      error = function(e) {
        cli::cli_abort(c("Failed to parse {.arg time_of_day} as a date time object", ">" = e[["message"]]), call = caller)
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
}


is_utc <- function(x, arg = rlang::caller_arg(x)) {
  if (!arcgisutils::is_date(x)) {
    cli::cli_abort("{.arg {arg}} is not a date or datetime")
  }
  "UTC" %in% attr(as.POSIXlt(x), "tzone")
}


validate_u_turns <- function(x, error_arg = rlang::caller_arg(), error_call = rlang::caller_call()) {
  if (is.null(x)) return(x)
  x <- rlang::arg_match(
    x,
    values = c("allow_backtrack", "deadend_intersection", "deadend", "no_backtrack"),
    error_call = error_call, error_arg = error_arg
  )

  lu <- setNames(
    c("esriNFSBAllowBacktrack", "esriNFSBAtDeadEndsAndIntersections", "esriNFSBAtDeadEndsOnly", "esriNFSBNoBacktrack"),
    c("allow_backtrack", "deadend_intersection", "deadend", "no_backtrack")
  )

  unname(lu[x])
}
