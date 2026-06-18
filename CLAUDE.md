# CLAUDE.md

## Imperatives

- Never attempt to run bash commands
- Never try to run git commands
- Never try and search outside of this workspace
- Never try to run any cargo commands whatsoever
- Never use `.unwrap()` or `.expect()`
- Never duplicate code unless you ask me for approval and get consent
- Never search code to get an answer. Always ask me for help insteaad.
- Never guess method implementations.

## Style & Guide

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
- **Never modify code outside the file currently being implemented.** If
  you spot a duplicate or improvement elsewhere, mention it once to the
  user and move on. Do not touch it.

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

## Async GP job URL construction

The `helperServices` URL for each endpoint varies — some point to the GP
server root, others point directly to the task:

- `asyncRoute$url` → GP server root (e.g. `.../Route/GPServer`) — append
  the task name with `httr2::req_url_path_append()$url`
- `asyncServiceArea$url` → already points to the task
  (e.g. `.../ServiceAreas/GPServer/GenerateServiceAreas`) — use directly
  as `base_url`, do NOT append

Always verify by printing `job$base_url` after constructing the job. If
the task name appears twice in the URL, the URL was already a task URL.

## Writing result parsers for GP jobs

**Never write the result parser before seeing actual results.** The
workflow is:

1.  Implement the job constructor *without* a `result_fn` — the default
    [`RcppSimdJson::fparse()`](https://rdrr.io/pkg/RcppSimdJson/man/fparse.html)
    runs and returns a data frame.
2.  Ask the user to run the job and share `result$paramName` and
    `result$dataType` (as vectors, not the full data frame).
3.  Map each index (0-based) to its type:
    - `GPFeatureRecordSetLayer` → `try_parse(json, "/N/value")`
    - `GPRecordSet` → `try_parse(json, "/N/value")`
    - `GPBoolean` / `GPString` →
      `RcppSimdJson::fparse(json, query = "/N/value")`
    - `GPDataFile` → `RcppSimdJson::fparse(json, query = "/N/value")` —
      these return file URLs, include them, do NOT skip
4.  The raw results JSON is a **0-indexed array** — use numeric JSON
    Pointer paths (`"/0/value"`, `"/1/value"`, etc.) not name-based
    paths.
5.  Write `parse_<endpoint>_results(json)` returning a `compact()` named
    list with snake_case keys derived from `paramName`.
6.  Pass the function as the `result_fn` positional argument (third arg)
    in the `$new()` call. `token` must be passed as a named argument.
