#' @export
as_od_points <- function(x, ...) {
  UseMethod("as_od_points")
}

# Fields 
# ObjectID
# Name - character
# TargetDestinationCount - numeric (cast as integer)
# Cutoff - Numeric
# CurbApproach - valid values are c(NA, 0L, 1L, 2L, 3L) (cast as integer)
# Bearing - numeric
# BearingTol - numeric range in (0, 180)
# NavLatency - only used when Bearing & BearingTol


#' @export
as_od_points.sfc <- function(x, ...) {
  check_od_points(x)
  arcgisutils::as_esri_featureset(x)
}

check_od_points <- function(x, error_call = rlang::caller_call()) {
  if (!inherits(x, "sfc_POINT")) {
    cli::cli_abort(
      "Origins and destinations must be point geometries not {obj_type_friendly(x)}",
      call = error_call
    )
  }
  
  if (is.na(sf::st_crs(x))) {
    cli::cli_abort(c("!" = "`crs` is not set. Please set the crs."))
  }

  if (st_count(x) > 1000L) {
    cli::cli_abort(
      c(
        "The maximum number of points supported is {.val {1000L}}",
        "i" = "If this is a limitation for you, please report it",
        " " = "{.url https://github.com/r-arcgis/arcgisrouting/issues}"
      ),
      call = error_call
    )
  }
}

#' @export
as_od_points.sf <- function(x, verbose = TRUE, ...) {
  check_od_points(sf::st_geometry(x))

  od_point_colname_lu <- c(
    "name" = "Name", 
    "object_id" = "ObjectID",
    "objectID" = "ObjectID",
    "objectid" = "ObjectID",
    "ObjectID" = "ObjectID",
    "bearing" = "Bearing",
    "bearing_tol" = "BearingTol",
    "nav_latency" = "NavLatency",
    "cutoff" = "Cutoff",
    "n_dests" = "TargetDestinationCount",
    "curb_approach" = "CurbApproach"
  )
  

  # valid columns
  common_cols <- intersect(
    names(od_point_colname_lu),
    colnames(x)
  )

  common_lu_vals <- na.omit(od_point_colname_lu[colnames(x)])
  n_common <- length(common_lu_vals)
  
  # inform
  if (verbose && n_common > 0) {
    cli::cli_alert_info("Using the provided attributes: {.col {common_cols}}")
  }


  if (n_common == 0) {
    return(as_od_points(sf::st_geometry(x)))
  }

  # check for dupe cols
  n_col <- table(common_lu_vals) 
  are_dupes <- n_col > 1
  dupe_cols <- names(n_col)[are_dupes]
  if (any(are_dupes)) {
    cli::cli_abort("Found duplicate column{?s}: {dupe_cols}")
  }


  # we convert OID to character b/c of the way that extendr converts to list
  # if ("ObjectID" %in% common_lu_vals) {
  #   oid_idx <- which(common_lu_vals == "ObjectID")
  #   x[[oid_idx]] <- format(x[[oid_idx]], scientific = FALSE)
  # }

  if ("TargetDestinationCount" %in% common_lu_vals) {
    if (!is.numeric(x[["n_dests"]])) {
      cli::cli_abort("{.col n_dests} must be an integer vector")
    } else {
      x[["n_dests"]] <- as.integer(x[["n_dests"]]) 
    }
  }
  
  # validate the cutoff if it is present
  if ("Cutoff" %in% common_lu_vals) {
    if (!is.numeric(x[["cutoff"]])) {
      cli::cli_abort("{.col cutoff} must be a numeric vector")
    }
  }

  if ("Bearing" %in% common_lu_vals) {
    if (!is.numeric(x[["bearing"]])) {
      cli::cli_abort("{.col bearing} must be a numeric vector")
    }
  }

  if ("BearingTol" %in% common_lu_vals) {
    if (!is.numeric(x[["bearing_tol"]])) {
      cli::cli_abort("{.col BearingTol} must be a numeric vector")
    }

    b_range <- range(x[["bearing_tol"]])

    if (b_range[1] < 0 || b_range[2] > 180) {
      cli::cli_abort("{.col bearing_tol} must be between 0 and 180 inclusive")
    }
  }

  if ("NavLatency" %in% common_lu_vals && !("Bearing" %in% common_lu_vals || "BearingTol" %in% common_lu_vals) ) {
    cli::cli_abort(
      "{.col nav_latency} provided but not accompanied by {.col bearing} and {.col bearing_tol}"
    )
  }

  # subset and reorder and rename
  x <- x[common_cols]
  colnames(x) <- c(na.omit(common_lu_vals), "geometry")
  sf::st_geometry(x) <- "geometry"
  arcgisutils::as_esri_featureset(x)
}

