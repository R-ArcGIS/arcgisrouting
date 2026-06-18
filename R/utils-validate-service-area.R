validate_travel_direction <- function(
  x,
  error_arg = rlang::caller_arg(x),
  error_call = rlang::caller_call()
) {
  if (is.null(x)) {
    return(NULL)
  }
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

validate_multiple_facilities <- function(
  x,
  error_arg = rlang::caller_arg(x),
  error_call = rlang::caller_call()
) {
  if (is.null(x)) {
    return(NULL)
  }
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
  if (is.null(x)) {
    return(NULL)
  }
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
  esri_units <- c(
    "esriCentimeters",
    "esriDecimalDegrees",
    "esriDecimeters",
    "esriFeet",
    "esriInches",
    "esriKilometers",
    "esriMeters",
    "esriMiles",
    "esriMillimeters",
    "esriNauticalMiles",
    "esriPoints",
    "esriYards"
  )

  unit_names <- c(
    "centimeters",
    "decimaldegrees",
    "decimeters",
    "feet",
    "inches",
    "kilometers",
    "meters",
    "miles",
    "millimeters",
    "nautical_miles",
    "points",
    "yards"
  )

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
  check_number_decimal(
    x[["distance"]],
    min = 0,
    call = error_call,
    arg = error_arg
  )
  x[["units"]] <- validate_units(
    x[["units"]],
    error_arg = error_arg,
    error_call = error_call
  )

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
    x,
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
