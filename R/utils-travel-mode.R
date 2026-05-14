# library(arcgisutils)

# token <- auth_user()
# modes <- retrieve_travel_modes(token)
# x <- modes$defaultTravelMode

.validate_travel_mode <- function(
  x,
  error_arg = rlang::caller_arg(x),
  error_call = rlang::caller_call(),
  token = arc_token()
) {
  # early exit for NULL
  if (is.null(x)) {
    return(NULL)
  }

  # check that x is indeed a string
  check_string(x, arg = error_arg, call = error_call)

  # set x to lowercase to ensure no cap errors
  x <- tolower(x)

  # fetch travel modes and create a lookup vector
  modes_raw <- retrieve_travel_modes(token, error_call)
  supported_modes <- modes_raw$supportedTravelModes
  mode_ids <- supported_modes$id
  mode_names <- supported_modes$name

  # check if `x` is in the names or the IDs
  mode_idx <- which(x == tolower(mode_ids))
  mode_name_idx <- which(x == tolower(mode_names))

  if (rlang::is_empty(mode_idx) && rlang::is_empty(mode_name_idx)) {
    bullets <- setNames(
      sprintf("{.val %s}: {.val %s}", mode_names, mode_ids),
      rep("*", length(mode_ids))
    )
    cli::cli_abort(
      c(
        "{.arg {error_arg}} is not a valid {.arg travel_mode}. Provide the mode ID or name directly.",
        "i" = "Valid travel modes are:",
        bullets
      ),
      call = error_call
    )
  }

  id <- na.omit(c(mode_idx, mode_name_idx))
  mode_attrs <- supported_modes[id, ]

  mode_list <- as.list(mode_attrs)
  mode_list$attributeParameterValues <- mode_list$attributeParameterValues[[1]]
  mode_list$restrictionAttributeNames <- mode_list$restrictionAttributeNames[[1]]

  yyjsonr::write_json_str(mode_list, auto_unbox = TRUE, dataframe = "rows")
}


.get_travel_modes <- function(token = arc_token()) {
  modes <- retrieve_travel_modes(token, rlang::caller_call())
  modes[["supportedTravelModes"]][["name"]]
}


#' Get available travel modes
#' @export
get_travel_modes <- memoise::memoise(.get_travel_modes)


# validate_travel_mode <- memoise::memoise(.validate_travel_mode)
validate_travel_mode <- .validate_travel_mode

names_swap <- function(x, error_call = rlang::caller_call()) {
  if (!rlang::is_named(x)) {
    cli::cli_abort(
      "Expected named list. This was unexpercted please create a GitHub issue {.url https://github.com/r-arcgis/arcgisrouting/issues/new}.",
      call = error_call
    )
  }
  setNames(names(x), x)
}
