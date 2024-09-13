# Each organization has different methods of travel modes that are available
# to them. Run this to figure that out.
# https://route.arcgis.com/arcgis/rest/services/World/OriginDestinationCostMatrix/NAServer/OriginDestinationCostMatrix_World/retrieveTravelModes?f=json&token=


#' Retrieve Available Travel Modes
#'
.retrieve_travel_modes <- function(token = arcgisutils::arc_token(), error_call = rlang::caller_call()) {
  # get portal metadata
  meta <- arcgisutils::arc_self_meta(token = token)

  # fetch the odCostMatrix url
  # idk if this is different than the async one—but I don't think so
  od_cost_url <- meta$helperServices$odCostMatrix$url

  # if this is null stop
  if (is.null(od_cost_url)) {
    cli::cli_abort(
      "Cannot find origin-destination cost matrix service associated with your token",
      call = error_call
    )
  }

  # fetch the travel modes
  travel_modes <- arc_base_req(
    od_cost_url,
    token,
    path = "retrieveTravelModes",
    query = c(f = "json")
  ) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    RcppSimdJson::fparse()

  # check if there is an error
  detect_errors(travel_modes)

  # if travel modes were not found, then we abort
  if (is.null(travel_modes[["supportedTravelModes"]])) {
    cli::cli_abort(
      "Supported travel modes were not found",
      call = error_call
    )
  }

  # desired column order—put the name and id first.
  desired_col_order <- union(
    c("name", "id", "description"),
    colnames(travel_modes[["supportedTravelModes"]])
  )


  # we make the travel modes a tbl for printing
  travel_modes[["supportedTravelModes"]] <- data_frame(
    travel_modes[["supportedTravelModes"]]
  )[desired_col_order]

  travel_modes
}
#' @export
retrieve_travel_modes <- memoise::memoise(.retrieve_travel_modes)