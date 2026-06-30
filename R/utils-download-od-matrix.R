#' Download Origin-Destination Cost Matrix Results
#'
#' Downloads and unzips the result file produced by a completed
#' origin-destination cost matrix geoprocessing job, reading each CSV output
#' into a named list of data frames with snake_case column names.
#'
#' @param job A completed origin-destination cost matrix job object.
#'
#' @returns A named list of data frames, one per CSV output, with snake_case
#'   names derived from the output file names.
#'
#' @examples
#' \dontrun{
#' # This example is not executed since it requires a network connection
#' # to ArcGIS Online and a valid authentication token
#' library(sf)
#' library(arcgisutils)
#' set_arc_token(auth_user())
#'
#' origins <- st_sfc(
#'   st_point(c(-122.4194, 37.7749)),
#'   st_point(c(-122.4313, 37.7793)),
#'   crs = 4326
#' )
#'
#' destinations <- st_sfc(
#'   st_point(c(-122.4083, 37.7858)),
#'   st_point(c(-122.4000, 37.7900)),
#'   crs = 4326
#' )
#'
#' job <- od_cost_matrix_job(origins, destinations)
#' job$start()
#' job$await()
#'
#' download_od_results(job)
#' }
#'
#' @family async
#' @family od
#' @export
download_od_results <- function(job) {
  rlang::check_installed(c("readr", "heck"))

  # check that the object is an esri job
  if (!inherits(job, "gp_job")) {
    cli::cli_abort("Expected {.cls gp_job} found {obj_type_friendly(job)}.")
  }

  # fetch the results data.frame
  cli::cli_alert_info("Checking job status...")
  status <- job$job_status

  # check that the status is a success
  if (!status$jobStatus == "esriJobSucceeded") {
    cli::cli_abort(
      "Job did not succeed. The status is {.val {results$jobStatus}}."
    )
  }

  cli::cli_alert_success("Job succeed. Downloading results...")
  results <- job$job_results

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
  csv_files <- list.files(
    dirname(tmp),
    pattern = "*.csv$",
    full.names = TRUE
  )
  csv_names <- heck::to_snek_case(
    gsub("ODCMOutput", "", tools::file_path_sans_ext(basename(csv_files)))
  )

  lapply(setNames(csv_files, csv_names), function(.csv) {
    res_df <- readr::read_csv(.csv, show_col_types = FALSE)
    colnames(res_df) <- heck::to_snek_case(colnames(res_df))
    res_df
  })
}
