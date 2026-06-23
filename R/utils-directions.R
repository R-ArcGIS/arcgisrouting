validate_directions_style_name_async <- function(
  x,
  error_arg = rlang::caller_arg(x),
  error_call = rlang::caller_env()
) {
  if (is.null(x)) {
    return(NULL)
  }

  x <- rlang::arg_match0(
    x,
    values = c("desktop", "navigation"),
    arg_nm = error_arg,
    error_call = error_call
  )

  lu <- c(
    "desktop" = "NA Desktop",
    "navigation" = "NA Navigation"
  )

  unname(lu[x])
}
