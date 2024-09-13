# Internal S7 class to validate the status of the GP job
class_job_status <- S7::new_class(
  "gp_job_status",
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
# s7 class which validates the esri form parameters 
esri_form_params <- S7::new_class(
  "gp_job_params",
  properties = list(params = S7::class_list),
  validator = function(self) {
    lapply(
      self@params,
      check_string,
      allow_null = TRUE,
      allow_na = FALSE,
      call = rlang::caller_call()
    )
    # idk why this has to return NULL
    return(NULL)
  }
)

# used for active bindings to check the job status
.job_status <- function() {
  # if there is a NULL job ID we abort 
  if (is.null(self$id)) {
    cli::cli_abort(
      c("There is no job ID present.", ">" = " Have you started the job with `x$start()`?")
    )
  }

  # check the status
  resp <- arcgisutils::arc_base_req(
    self$base_url,
    token = private$token,
    path = c("jobs", self$id),
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
  
  class_job_status(res[["jobStatus"]])
  
}

.job_results <- function() {
    # if there is a NULL job ID we abort 
    if (is.null(self$id)) {
      cli::cli_abort(
        c("There is no job ID present.", ">" = " Have you started the job with `x$start`?")
      )
    }
  
    # check the status
    resp <- arcgisutils::arc_base_req(
      self$base_url,
      token = private$token,
      path = c("jobs", self$id, "results"),
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


#' Create a Geoprocessing Job
#' 
#' The `gp_job` class is used to interact with Geoprocessing Services in 
#' ArcGIS Online and Enterprise. 
#' @export
esri_job <- R6::R6Class(
  "gp_job",
  public = list(
    #' @field base_url the URL of the job service (without `/submitJob`)
    base_url = NULL, 
    #' @field id the ID of the started job. `NULL` `self$start()` has not been called.
    id = NULL, 
    #' @param base_url the URL of the job endpoint (without `/submitJob`)
    #' @param params a named list where each element is a scalar character
    #' @param token default [arcgisutils::arc_token()]. The token to be used with the job. 
    initialize = function(base_url, params = list(), token = arc_token()) {
      # use S7 to validate the form parameters
      self$base_url <- base_url
      private$.params <- esri_form_params(params)
      private$token <- token
      self
    },
    #' @description  Starts the job by calling the `/submitJob` endpoint. This also sets the public field `id`.
    start = function() {
      # TODO make it possible to only do this once
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
      self$id <- res$jobId
      self

    },
    #' @description Cancels a job by calling the `/cancel` endpoint.
    cancel = function() {
      resp <- arcgisutils::arc_base_req(
        self$base_url,
        token = private$token,
        path = c("jobs", self$id, "cancel"),
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
      self$id <- res$jobId
      self

    }
  ),
  private = list(
    .params = NULL,
    .status = .job_status,
    token = NULL
  ),
  active = list(
    #' @field params returns an S7 object of class [esri_form_params] the list can be accessed via `self$params@params`.
    params = function() {
      private$.params
    },
    status = function() private$.status(),
    results = .job_results
  )
)

