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
- Never try to align arguments. Never add unecessary whitepace for stylistic reasons.

## Function arguments

**Naming**: snake_case R names mapping to API names internally. Reuse the same name whenever the concept is the same across functions.

**Ordering**: required spatial inputs first, core behavioral params, optional feature inputs, output control params, `token` always last.

**Defaults**: `NULL` for anything optional — dropped via `compact()`. Non-NULL defaults only when omitting would surprise the user.

**Validation**: every validated param gets a reusable function, never inline. Shared validators go in `utils-*.R`, function-specific stay in their file. Pattern: `NULL` early return → scalar type check → `arg_match` → lookup vector to API value. Never duplicate a validator that already exists.

**Spatial vs tabular inputs**: use `sf`/`sfc` whenever geometry is involved. Use plain `data.frame` for attribute-only tables. Both get a dedicated `as_*()` converter using `UseMethod` dispatch — recognizing snake_case column names, validating types, renaming to API names, serializing via `arcgisutils::as_esri_featureset()`.