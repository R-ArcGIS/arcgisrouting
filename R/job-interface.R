class_job_status <- S7::new_class(
  "esri_job_status",
  properties = list(status = S7::class_character),
  validator = function(self) {
    job_statuses <- c(
      "esriJobSubmitted",
      "esriJobWaiting",
      "esriJobExecuting",
      "esriJobSucceeded",
      "esriJobFailed",
      "esriJobTimedOut",
      "esriJobCancelling",
      "esriJobCancelled"
    )

    # permit length 0 
    if (length(self@status) == 0L) return(NULL)
    # if it is longer than 0, it should have been set to something
    if (length(self@status) > 1) {
      cli::cli_abort("Job status must be a scalar value")
    } else if (!self@status %in% job_statuses) {
      cli::cli_abort("Job status must be one of {.val {job_statuses}}")
    } 
    
  }
)

# S7 class to represent all of the job parameters we might have 
esri_form_params <- S7::new_class(
  "esri_job_params",
  properties = list(params = S7::class_list),
  validator = function(self) {
    lapply(self@params, check_string, allow_null = TRUE, allow_na = FALSE, call = rlang::caller_call())
    # idk why this has to return NULL
    return(NULL)
  }
)

.job_status <- function() {
  # if there is a NULL job ID we abort 
  if (is.null(self$job_id)) {
    cli::cli_abort(
      c("There is no job ID present.", ">" = " Have you started the job with `x$start_job()`?")
    )
  }

  # check the status
  resp <- arcgisutils::arc_base_req(
    self$base_url,
    token = private$token,
    path = c("jobs", self$job_id),
    query = c(f = "json")
  ) |> 
    httr2::req_error(is_error = function(e) FALSE) |> 
    httr2::req_perform() |> 
    httr2::resp_body_string()

  # read the string 
  res <- yyjsonr::read_json_str(resp)

  # check for errors
  # due to use of {yyjsonr} have to adjust....
  if (!is.null(res[["error"]])) {
    if (is.list(res$error$details)) {
      res[["error"]][["details"]] <- NULL
      detect_errors(res)
    }
    detect_errors(res)
  }
  
  self$status <-  class_job_status(res[["jobStatus"]])
  res
}

.job_results <- function() {
    # if there is a NULL job ID we abort 
    if (is.null(self$job_id)) {
      cli::cli_abort(
        c("There is no job ID present.", ">" = " Have you started the job with `x$start_job()`?")
      )
    }
  
    # check the status
    resp <- arcgisutils::arc_base_req(
      self$base_url,
      token = private$token,
      path = c("jobs", self$job_id, "results"),
      query = c(f = "json")
    ) |> 
      httr2::req_error(is_error = function(e) FALSE) |> 
      httr2::req_perform() |> 
      httr2::resp_body_string()
  
    # read the string 
    res <- yyjsonr::read_json_str(resp)
  
    # check for errors
    # due to use of {yyjsonr} have to adjust....
    if (!is.null(res[["error"]])) {
      if (is.list(res$error$details)) {
        res[["error"]][["details"]] <- NULL
        detect_errors(res)
      }
      detect_errors(res)
    }
    
    data_frame(res)
}


esri_job <- R6::R6Class(
  "esri_job",
  public = list(
    base_url = NULL, 
    job_id = NULL, 
    status = NULL, 
    initialize = function(base_url, params = list(), token = arc_token()) {
      # use S7 to validate the form parameters
      self$base_url <- base_url
      private$.params <- esri_form_params(params)
      private$token <- token
      self
    },
    start_job = function() {
      resp <- arcgisutils::arc_base_req(
        self$base_url,
        token = private$token,
        path = "submitJob"
      ) |> 
        httr2::req_body_form(!!!private$.params@params) |> 
        httr2::req_error(is_error = function(e) FALSE) |> 
        httr2::req_perform()

      res <- yyjsonr::read_json_str(httr2::resp_body_string(resp))
      # check for errors
      # due to use of {yyjsonr} have to adjust....
      if (!is.null(res[["error"]])) {
        if (is.list(res$error$details)) {
          res[["error"]][["details"]] <- NULL
          detect_errors(res)
        }
        detect_errors(res)
      }

      print(res)
      self$status <- class_job_status(status = res$jobStatus)
      self$job_id <- res$jobId
      self
    },
    cancel_job = function() {
      resp <- arcgisutils::arc_base_req(
        self$base_url,
        token = private$token,
        path = c("jobs", self$job_id, "cancel"),
        query = c(f = "json")
      ) |> 
        httr2::req_body_form(!!!private$.params@params) |> 
        httr2::req_error(is_error = function(e) FALSE) |> 
        httr2::req_perform()


      res <- yyjsonr::read_json_str(httr2::resp_body_string(resp))
      # check for errors
      # due to use of {yyjsonr} have to adjust....
      if (!is.null(res[["error"]])) {
        if (is.list(res$error$details)) {
          res[["error"]][["details"]] <- NULL
          detect_errors(res)
        }
        detect_errors(res)
      }

      self$status <- class_job_status(status = res$jobStatus)
      self$job_id <- res$jobId
      self

    }
  ),
  private = list(
    .params = NULL,
    .job_status = .job_status,
    token = NULL
  ),
  active = list(
    params = function() {
      private$.params
    },
    job_status = function() private$.job_status(),
    job_results = .job_results
  )
)

