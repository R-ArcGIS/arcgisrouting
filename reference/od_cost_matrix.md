# Origin-Destination Cost Matrix

Creates an origin-destination cost matrix containing the travel cost
between every origin and destination.

## Usage

``` r
od_cost_matrix(
  origins,
  destinations = origins,
  travel_mode = NULL,
  default_cutoff = NULL,
  default_target_destination_count = NULL,
  output_type = NULL,
  time_of_day = NULL,
  u_turns = NULL,
  use_hierarchy = NULL,
  impedance = NULL,
  accumulate_impedance = NULL,
  restrictions = NULL,
  attribute_parameter_values = NULL,
  point_barriers = NULL,
  line_barriers = NULL,
  polygon_barriers = NULL,
  return_geometry = character(0),
  ignore_invalid_locations = TRUE,
  return_empty_results = FALSE,
  geometry_precision = NULL,
  locate_settings = NULL,
  crs = 4326,
  token = arcgisutils::arc_token()
)
```

## Arguments

- origins:

  an `sf` or `sfc` object containing point geometries representing the
  starting points.

- destinations:

  default `origins`. An `sf` or `sfc` object containing point geometries
  representing the ending points.

- default_cutoff:

  default `NULL`. A numeric scalar. The travel time or distance value at
  which to stop searching for facilities.

- default_target_destination_count:

  default `NULL`. An integer scalar. The maximum number of destinations
  to find per origin.

- output_type:

  default `NULL`. A scalar character. One of `"no_lines"`,
  `"straight_lines"`, `"sparse_matrix"`. Controls whether route geometry
  is returned.

- attribute_parameter_values:

  default `NULL`. A list of objects. Additional values required by an
  attribute or restriction.

- return_geometry:

  default `character(0)`. A character vector. Valid values: `"origins"`,
  `"destinations"`, `"barriers"`, `"polyline_barriers"`,
  `"polygon_barriers"`.

- return_empty_results:

  default `FALSE`. A logical scalar. Returns empty results instead of an
  error on failure.

- geometry_precision:

  default `NULL`. An integer scalar. Decimal places for x and y values
  in response geometries.

- locate_settings:

  default `NULL`. A list controlling how inputs are located on the
  network.

- crs:

  default `4326`. The coordinate reference system of the output
  geometries. Passed to
  [`arcgisutils::as_spatial_reference()`](https://rdrr.io/pkg/arcgisutils/man/gp_params.html).

## Value

A named list. Elements present depend on `return_geometry` and
`output_type`:

- `od_cost_matrix`: nested cost matrix returned when
  `output_type = "sparse_matrix"`. A list with `costAttributeNames` and,
  per origin ID, a list mapping destination ID to its cost values.

- `od_lines`: OD cost matrix features

- `origins`: origin features

- `destinations`: destination features

- `barriers`: point barrier features

- `polyline_barriers`: polyline barrier features

- `polygon_barriers`: polygon barrier features

- `messages`: status and warning messages from the service

## References

[API
Reference](https://developers.arcgis.com/rest/routing/travelCostMatrix-service-direct/)

## See also

Other direct:
[`find_closest_facilities()`](http://r.esri.com/arcgisrouting/reference/find_closest_facilities.md),
[`find_routes()`](http://r.esri.com/arcgisrouting/reference/find_routes.md),
[`find_service_areas()`](http://r.esri.com/arcgisrouting/reference/find_service_areas.md),
[`route_vehicles()`](http://r.esri.com/arcgisrouting/reference/route_vehicles.md),
[`snap_to_roads()`](http://r.esri.com/arcgisrouting/reference/snap_to_roads.md)

Other od:
[`download_od_results()`](http://r.esri.com/arcgisrouting/reference/download_od_results.md),
[`od_cost_matrix_job()`](http://r.esri.com/arcgisrouting/reference/od_cost_matrix_job.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(sf)
library(arcgisutils)
set_arc_token(auth_user())

origins <- st_sfc(
  st_point(c(-122.4194, 37.7749)),
  st_point(c(-122.4313, 37.7793)),
  crs = 4326
)

destinations <- st_sfc(
  st_point(c(-122.4083, 37.7858)),
  st_point(c(-122.4000, 37.7900)),
  st_point(c(-122.4561, 37.7513)),
  crs = 4326
)

result <- od_cost_matrix(
  origins = origins,
  destinations = destinations
)

result
} # }
```
