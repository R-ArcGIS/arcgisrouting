# CLAUDE.md

- This is the repository for the R package
  [arcgisrouting](http://r.esri.com/arcgisrouting/) which is a part of
  the R-ArcGIS Bridge.
- It provides bindings to the ArcGIS Routing REST API documented at
  <https://developers.arcgis.com/rest/routing/>.
- It uses [arcgisutils](https://github.com/R-ArcGIS/arcgisutils) for
  Esri JSON handling including date conversions and geoprocessing
  parameters.
- We use strong type checking via standalone type checks defined at
  R/import-standalone-types-check.R
- We always use [rlang](https://rlang.r-lib.org) for low level actions
- Errors and warnings are created using [cli](https://cli.r-lib.org)
- Wrting duplicate functions or duplicate vectors is banned
- When validating a type or object from always create a reusable
  function.
- Comments should only explain justification and reasoning.
  - Comments shuold never describe what the function call is.
- [`sapply()`](https://rdrr.io/r/base/lapply.html) is banned always use
  a stronger typed [`apply()`](https://rdrr.io/r/base/apply.html) or
  [`lapply()`](https://rdrr.io/r/base/lapply.html)
- Always use `compact()` to remove NULL elements from a list
- Always use `#' @inheritParams` for documentation whenever possible.
  Never duplicate documentation manually
- Never try to align arguments. Never add unecessary whitepace for
  stylistic reasons.

## Function arguments

**Naming**: snake_case R names mapping to API names internally. Reuse
the same name whenever the concept is the same across functions.

**Ordering**: required spatial inputs first, core behavioral params,
optional feature inputs, output control params, `token` always last.

**Defaults**: `NULL` for anything optional — dropped via `compact()`.
Non-NULL defaults only when omitting would surprise the user.

**Validation**: every validated param gets a reusable function, never
inline. Shared validators go in `utils-*.R`, function-specific stay in
their file. Pattern: `NULL` early return → scalar type check →
`arg_match` → lookup vector to API value. Never duplicate a validator
that already exists.

**Spatial vs tabular inputs**: use `sf`/`sfc` whenever geometry is
involved. Use plain `data.frame` for attribute-only tables. Both get a
dedicated `as_*()` converter using `UseMethod` dispatch — recognizing
snake_case column names, validating types, renaming to API names,
serializing via
[`arcgisutils::as_esri_featureset()`](https://rdrr.io/pkg/arcgisutils/man/featureset.html).

## Writing result parsers for GP jobs

**Never write the result parser before seeing actual results.** The
workflow is:

1.  Implement the job constructor *without* a `result_fn` — the default
    [`RcppSimdJson::fparse()`](https://rdrr.io/pkg/RcppSimdJson/man/fparse.html)
    runs and returns a data frame.
2.  Ask the user to run the job and share `job$results` so we can see
    the `paramName`, `dataType`, and `value` columns.
3.  Inspect `res$dataType` to identify which rows are
    `GPFeatureRecordSetLayer` (need `try_parse()`), which are scalar
    types like `GPBoolean`/`GPString` (use
    [`RcppSimdJson::fparse()`](https://rdrr.io/pkg/RcppSimdJson/man/fparse.html)),
    and which are `GPDataFile` (skip — will be `NULL`).
4.  The raw results JSON is a **0-indexed array** — use numeric JSON
    Pointer paths (`"/0/value"`, `"/1/value"`, etc.) not name-based
    paths.
5.  Write `parse_<endpoint>_results(json)` using
    `try_parse(json, "/N/value")` for feature layers and
    `RcppSimdJson::fparse(json, query = "/N/value")` for scalars. Return
    a `compact()` named list with snake_case keys.
6.  Pass the function as the `result_fn` argument in the `$new()` call.
