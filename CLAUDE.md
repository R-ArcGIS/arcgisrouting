# CLAUDE.md

- This is the repository for the R package `{arcgisrouting}` which is a part of the R-ArcGIS Bridge. 
- It provides bindings to the ArcGIS Routing REST API documented at https://developers.arcgis.com/rest/routing/.
- It uses `{arcgisutils}` for Esri JSON handling including date conversions and geoprocessing parameters. 
- We use strong type checking via standalone type checks defined at R/import-standalone-types-check.R
- We always use `{rlang}` for low level actions
- Errors and warnings are created using `{cli}`
- Wrting duplicate functions or duplicate vectors is banned
- When validating a type or object from always create a reusable function.
- Comments should only explain justification and reasoning.
  - Comments shuold never describe what the function call is.
- `sapply()` is banned always use a stronger typed `apply()` or `lapply()`
- Always use `compact()` to remove NULL elements from a list
- Always use `#' @inheritParams` for documentation whenever possible. Never duplicate documentation manually