# Find Closest Facilities

Finds one or more nearby facilities from incidents based on travel time
or distance.

## Usage

``` r
find_closest_facilities(
  incidents,
  facilities,
  travel_mode = NULL,
  default_target_facility_count = NULL,
  travel_direction = NULL,
  default_cutoff = NULL,
  time_of_day = NULL,
  time_of_day_usage = NULL,
  u_turns = NULL,
  use_hierarchy = NULL,
  impedance = NULL,
  accumulate_impedance = NULL,
  restrictions = NULL,
  attribute_parameter_values = NULL,
  point_barriers = NULL,
  line_barriers = NULL,
  polygon_barriers = NULL,
  return_geometry = c("facilities", "incidents"),
  directions_language = NULL,
  directions_output_type = NULL,
  directions_style = NULL,
  directions_length_units = NULL,
  directions_time_attribute = NULL,
  output_lines = "true_shape",
  ignore_invalid_locations = TRUE,
  preserve_object_id = FALSE,
  return_empty_results = FALSE,
  output_geometry_precision = 10,
  output_geometry_precision_units = "meters",
  geometry_precision = NULL,
  geometry_precision_m = NULL,
  locate_settings = NULL,
  crs = 4326,
  token = arcgisutils::arc_token()
)
```

## Arguments

- incidents:

  an `sf` or `sfc` object containing point geometries representing the
  locations to search from.

- facilities:

  an `sf` or `sfc` object containing point geometries representing the
  facilities to search for.

- travel_mode:

  Character. The name of the travel mode to use. See
  [`get_travel_modes()`](http://r.esri.com/arcgisrouting/reference/get_travel_modes.md)
  for available options. Default: `NULL`.

- default_target_facility_count:

  default `NULL`. An integer scalar. The number of closest facilities to
  find per incident.

- travel_direction:

  default `"away"`. A scalar character. One of `"away"` (away from
  facility) or `"towards"` (toward facility).

- default_cutoff:

  default `NULL`. A numeric scalar. The travel time or distance value at
  which to stop searching for facilities.

- time_of_day_usage:

  default `NULL`. A scalar character. One of `"start_time"` or
  `"end_time"`. Specifies whether `time_of_day` represents departure or
  arrival time.

- use_hierarchy:

  Logical. Whether to use hierarchy when finding routes. Default:
  `NULL`.

- restrictions:

  Character vector. Restriction names to honor. Default: `NULL`.

- attribute_parameter_values:

  default `NULL`. A list of objects. Additional values required by an
  attribute or restriction.

- polygon_barriers:

  Polygon barriers as `sf` or `sfc` object. Default: `NULL`.

- return_geometry:

  default `c("facilities", "incidents")`. A character vector. Valid
  values: `"cf_routes"`, `"facilities"`, `"incidents"`, `"directions"`,
  `"barriers"`, `"polyline_barriers"`, `"polygon_barriers"`,
  `"traversed_edges"`, `"traversed_junctions"`, `"traversed_turns"`. Use
  `"everything"` to return all.

- directions_language:

  Character. Language code for directions (e.g., `"en"`). Default:
  `"en"`.

- directions_output_type:

  default `NULL`. A scalar character. One of `"standard"`, `"complete"`,
  `"complete_no_events"`, `"instructions_only"`, `"summary_only"`,
  `"feature_sets"`.

- directions_style:

  default `NULL`. A scalar character. One of `"desktop"`,
  `"navigation"`, `"campus"`.

- directions_length_units:

  default `NULL`. A scalar character. One of `"miles"`, `"kilometers"`,
  `"feet"`, `"meters"`, `"yards"`, `"nautical_miles"`.

- directions_time_attribute:

  default `NULL`. A scalar character. The time-based impedance attribute
  used for direction durations.

- output_lines:

  default `NULL` (no lines). A scalar character or `NULL`. One of
  `"true_shape"` or `"with_measure"`.

- ignore_invalid_locations:

  Logical. Whether to ignore invalid locations. Default: `TRUE`.

- preserve_object_id:

  default `FALSE`. A logical scalar. Preserves object IDs from input
  locations in the output.

- return_empty_results:

  default `FALSE`. A logical scalar. Returns empty results instead of an
  error on failure.

- output_geometry_precision:

  default `10`. A numeric scalar. Simplification tolerance applied to
  output geometry.

- output_geometry_precision_units:

  default `"meters"`. A scalar character. Units for
  `output_geometry_precision`. Same valid values as
  `trim_polygon_distance_units`.

- geometry_precision:

  default `NULL`. An integer scalar. Decimal places for x and y values
  in response geometries.

- geometry_precision_m:

  default `NULL`. A scalar character. Decimal places for m-values in
  response geometries.

- locate_settings:

  default `NULL`. A list controlling how inputs are located on the
  network.

- crs:

  default `4326`. The coordinate reference system of the output
  geometries. Passed to
  [`arcgisutils::as_spatial_reference()`](https://rdrr.io/pkg/arcgisutils/man/gp_params.html).

- token:

  Authorization token. Default:
  [`arcgisutils::arc_token()`](https://rdrr.io/pkg/arcgisutils/man/token.html).

## Value

A named list. Elements present depend on `return_geometry`:

- `cf_routes`: route features between incidents and facilities

- `facilities`: facility features

- `incidents`: incident features

- `direction_points`: point features for direction maneuvers

- `direction_lines`: line features for route segments

- `barriers`: point barrier features

- `polyline_barriers`: polyline barrier features

- `polygon_barriers`: polygon barrier features

- `traversed_edges`: traversed edge features

- `traversed_junctions`: traversed junction features

- `traversed_turns`: traversed turn features

- `messages`: status and warning messages from the service

## References

[API
Reference](https://developers.arcgis.com/rest/routing/closestFacility-service-direct/)

## See also

Other direct:
[`find_routes()`](http://r.esri.com/arcgisrouting/reference/find_routes.md),
[`find_service_areas()`](http://r.esri.com/arcgisrouting/reference/find_service_areas.md),
[`od_cost_matrix()`](http://r.esri.com/arcgisrouting/reference/od_cost_matrix.md),
[`route_vehicles()`](http://r.esri.com/arcgisrouting/reference/route_vehicles.md),
[`snap_to_roads()`](http://r.esri.com/arcgisrouting/reference/snap_to_roads.md)

Other closest facility:
[`find_closest_facilities_job()`](http://r.esri.com/arcgisrouting/reference/find_closest_facilities_job.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(sf)
library(arcgisutils)
set_arc_token(auth_user())

incidents <- st_sfc(st_point(c(-122.4496, 37.7467)), crs = 4326)

facilities <- st_sf(
  name = c("Station 11", "Station 20", "Station 24", "Station 39"),
  geometry = st_sfc(
    st_point(c(-122.4267, 37.7486)),
    st_point(c(-122.4561, 37.7513)),
    st_point(c(-122.4409, 37.7533)),
    st_point(c(-122.4578, 37.7407)),
    crs = 4326
  )
)

result <- find_closest_facilities(
  incidents = incidents,
  facilities = facilities,
  default_target_facility_count = 2,
  travel_direction = "away",
  default_cutoff = 3,
  return_geometry = "cf_routes",
  directions_length_units = "miles",
  crs = 3857
)

result
} # }
```
