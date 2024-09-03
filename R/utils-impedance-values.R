# known & valid impedance
valid_impedance <- c("travel_time", "minutes", "truck_travel_time", "truck_minutes", "walk_time", "miles", "kilometers")

impedance_param_names <- c("TravelTime", "Minutes", "TruckTravelTime", "TruckMinutes", "WalkTime", "Miles", "Kilometers")

impedance_lu <- setNames(impedance_param_names, valid_impedance)

validate_impedance_value <- function(
    x,
    error_arg = rlang::caller_arg(x),
    error_call = rlang::caller_call(),
    multiple = FALSE
) {
  # early return for NULL value
  if (is.null(x)) {
    return(x)
  }


  impedance <- rlang::arg_match(
    x,
    values = valid_impedance,
    multiple = multiple,
    error_arg = error_arg,
    error_call = error_call
  )

  unname(impedance_lu[impedance])
}