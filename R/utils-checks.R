validate_time_units <- function(
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
    values = c("seconds", "minutes", "hours", "days"),
    arg_nm = error_arg,
    error_call = error_call
  )

  lu <- c(
    "seconds" = "Seconds",
    "minutes" = "Minutes",
    "hours" = "Hours",
    "days" = "Days"
  )

  unname(lu[x])
}

#' Add `tbl` class to a data.frame
#'
#' When pillar is loaded, a data.frame will print like a tibble
#' but will not inherit the tibble class.
#'
#' @noRd
#' @keywords internal
data_frame <- function(x, call = rlang::caller_env()) {
  check_data_frame(x, call = call)
  structure(x, class = c("tbl", "data.frame"))
}
