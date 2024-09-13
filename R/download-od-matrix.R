
download_od_results <- function(job) {

  # check that the object is an esri job
  if (!inherits(job, "gp_job")) {
    cli::cli_abort("Expected {.cls gp_job} found {obj_type_friendly(job)}.")
  }

  # fetch the results data.frame
  cli::cli_alert_info("Checking job status...")
  status <- job$job_status
  
  # check that the status is a success 
  if (!status$jobStatus == "esriJobSucceeded") {
    cli::cli_abort("Job did not succeed. The status is {.val {results$jobStatus}}.")
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