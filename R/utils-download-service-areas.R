#' Download Service Area Results
#'
#' Downloads and unzips the result file produced by a completed service area
#' geoprocessing job, parsing each JSON output into a named list of data frames
#' with snake_case column names.
#'
#' @param job A completed service area job object.
#'
#' @returns A named list of data frames, one per JSON output, with snake_case
#'   names derived from the output file names.
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(arcgisutils)
#' set_arc_token(auth_user())
#'
#' facilities <- st_sfc(
#'   st_point(c(-122.4194, 37.7749)),
#'   st_point(c(-122.0312, 37.3318)),
#'   crs = 4326
#' )
#'
#' job <- find_service_areas_job(facilities, break_values = c(5, 10, 15))
#' job$start()
#' job$await()
#'
#' download_service_area_results(job)
#' }
#'
#' @family async
#' @family service area
#' @export
download_service_area_results <- function(job) {
  rlang::check_installed("heck")
  rlang::check_installed("brio")

  # check that the object is an esri job
  if (!inherits(job, "service_area_job")) {
    cli::cli_abort(
      "Expected {.cls service_area_job} found {obj_type_friendly(job)}."
    )
  }

  # fetch the results data.frame
  cli::cli_alert_info("Checking job status...")
  status <- job$status@status

  # check that the status is a success
  if (!status == "esriJobSucceeded") {
    cli::cli_abort(
      "Job did not succeed. The status is {.val {results$jobStatus}}."
    )
  }

  cli::cli_alert_success("Job succeed. Downloading results...")
  results <- job$results

  # figure out which one contains the file
  out_file_idx <- which(results$paramName == "Output_Result_File")

  # download the file
  resp <- arc_base_req(
    results$value[[out_file_idx]][["url"]],
    arc_token()
  ) |>
    httr2::req_perform()

  # check that it is a binary response
  if (!identical(httr2::resp_content_type(resp), "binary/octet-stream")) {
    cli::cli_abort("Found unexpected response. Unable to download results.")
  }

  cli::cli_alert_info("Unzipping results...")

  # write the binary response to a zip file in the temporary directory
  tmp <- tempfile(fileext = ".zip")
  writeBin(httr2::resp_body_raw(resp), tmp)
  # unzip the zip file
  unzip(tmp, exdir = dirname(tmp))

  # list the csv files
  json_files <- list.files(
    dirname(tmp),
    pattern = "*.json$",
    full.names = TRUE
  )

  json_names <- heck::to_snek_case(
    gsub("SAOutput", "", tools::file_path_sans_ext(basename(json_files)))
  )

  lapply(setNames(json_files, json_names), function(.json) {
    res_df <- arcgisutils::parse_esri_json(
      brio::read_file(.json)
    )
    colnames(res_df) <- heck::to_snek_case(colnames(res_df))
    res_df
  })
}
