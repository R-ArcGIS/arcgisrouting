# library(arcgisutils)

# token <- auth_user()
# modes <- retrieve_travel_modes(token)


get_travel_modes <- function(token = arc_token()) {
  modes <- retrieve_travel_modes(token, rlang::caller_call())
  modes <- modes[["supportedTravelModes"]][c("name", "id")]
}

validate_travel_mode <- function(
    x,
    error_arg = rlang::caller_arg(x),
    error_call = rlang::caller_call()
) {

  # early exit for NULL
  if (is.null(x)) return(NULL)
  
  # check that x is indeed a string
  check_string(x, arg = error_arg, call = error_call)
  
  # fetch travel modes 
  modes_raw <- retrieve_travel_modes(token, rlang::caller_call())
  modes <- modes_raw[["supportedTravelModes"]][c("name", "id")]

  # create lookup vectors 
  lu_name <- tibble::deframe(modes)
  lu_id <- names_swap(lu, error_call = error_call)

  # get the first non-NA value
  travel_mode_id <- unname(na.omit(c(lu_name[x], lu_name[lu_id[x]]))[1])

  if (length(travel_mode_id) != 1L || is.na(travel_mode_id)) {
    # provide informative error message
    to_print <- paste(names(lu_name) ,": {.val ", lu_name, "}", sep = "")
    names(to_print) <- rep("*", length(to_print))
    cli::cli_abort(
      c(
        "{.arg {error_arg}} is not a valid {.arg travel_mode}.",
        ">" = "Expected the name or ID of a travel mode",
        to_print, 
        "i" = "use {.fn get_travel_modes} to identify available travel modes."
      )
    )
  }

  mode_idx <- which(modes_raw[["supportedTravelModes"]][["id"]] == travel_mode_id)
  mode_attrs <- modes_raw[["supportedTravelModes"]][["attributeParameterValues"]][[mode_idx]]
  # convert to a json string
  yyjsonr::write_json_str(
    list("attributeParameterValues" = mode_attrs),
    auto_unbox = TRUE
  ) 
}

names_swap <- function(x, error_call = rlang::caller_call()) {
  if (!rlang::is_named(x)) {
    cli::cli_abort(
      "Expected name list. This was unexpercted please create a GitHub issue.",
      call = error_call
    )
  }
  setNames(names(x), x)
}
