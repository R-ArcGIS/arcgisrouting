# Solve Service Area

Solve Service Area

## Usage

``` r
find_service_areas(
  facilities,
  default_breaks = c(5, 10, 15),
  travel_mode = NULL,
  travel_direction = "away",
  time_of_day = NULL,
  output_polygons = "simplified",
  split_polygons_at_breaks = TRUE,
  overlap_polygons = TRUE,
  merge_similar_polygon_ranges = FALSE,
  trim_outer_polygons = TRUE,
  output_lines = NULL,
  split_lines_at_breaks = TRUE,
  overlap_lines = TRUE,
  return_geometry = "service_areas",
  u_turns = NULL,
  use_hierarchy = NULL,
  impedance = NULL,
  accumulate_impedance = NULL,
  restrictions = NULL,
  trim_polygon_distance = 100,
  trim_polygon_distance_units = "meters",
  point_barriers = NULL,
  line_barriers = NULL,
  polygon_barriers = NULL,
  ignore_invalid_locations = TRUE,
  output_geometry_precision = 10,
  output_geometry_precision_units = "meters",
  crs = 4326,
  token = arcgisutils::arc_token()
)
```

## Arguments

- facilities:

  an `sf` or `sfc` object containing point geometries representing the
  facilities around which service areas are generated.

- default_breaks:

  default `c(5, 10, 15)`. A numeric vector specifying the size and
  number of service areas to generate for each facility. Units are
  determined by the `impedance` parameter.

- travel_mode:

  Character. The name of the travel mode to use. See
  [`get_travel_modes()`](http://r.esri.com/arcgisrouting/reference/get_travel_modes.md)
  for available options. Default: `NULL`.

- travel_direction:

  default `"away"`. A scalar character. One of `"away"` (away from
  facility) or `"towards"` (toward facility).

- time_of_day:

  default `NULL`. A scalar date-time. Either a `POSIXt` scalar or a
  character string parseable by
  [`as.POSIXlt()`](https://rdrr.io/r/base/as.POSIXlt.html). The time and
  date at which travel begins.

- output_polygons:

  default `"simplified"`. A scalar character or `NULL` (no polygons).
  One of `"simplified"` or `"detailed"`.

- split_polygons_at_breaks:

  default `TRUE`. A logical scalar. When `TRUE`, service areas appear as
  rings between breaks. When `FALSE`, each area is a disk from the
  facility to the break.

- overlap_polygons:

  default `TRUE`. A logical scalar. Whether service area polygons from
  different facilities can overlap.

- merge_similar_polygon_ranges:

  default `FALSE`. A logical scalar. Whether service area polygons from
  different facilities with the same break value are merged into a
  single polygon.

- trim_outer_polygons:

  default `TRUE`. A logical scalar. Whether service areas are trimmed to
  lie within a distance of the network. Ignored when
  `use_hierarchy = TRUE`.

- output_lines:

  default `NULL` (no lines). A scalar character or `NULL`. One of
  `"true_shape"` or `"with_measure"`.

- split_lines_at_breaks:

  default `TRUE`. A logical scalar. Whether service area lines are split
  at break values.

- overlap_lines:

  default `TRUE`. A logical scalar. Whether service area lines from
  different facilities can overlap.

- return_geometry:

  default `"service_areas"`. A character vector. Valid values:
  `"service_areas"`, `"sa_lines"`, `"facilities"`, `"barriers"`,
  `"polyline_barriers"`, `"polygon_barriers"`.

- u_turns:

  default `NULL`. A scalar character. U-turn policy at junctions. One of
  `"allow_backtrack"`, `"deadend_intersection"`, `"deadend"`,
  `"no_backtrack"`.

- use_hierarchy:

  Logical. Whether to use hierarchy when finding routes. Default:
  `NULL`.

- impedance:

  default `NULL`. A scalar character. The impedance to minimize. One of
  `"travel_time"`, `"minutes"`, `"truck_travel_time"`,
  `"truck_minutes"`, `"walk_time"`, `"miles"`, `"kilometers"`.

- accumulate_impedance:

  default `NULL`. A character vector. Additional impedance values to
  accumulate.

- restrictions:

  Character vector. Restriction names to honor. Default: `NULL`.

- trim_polygon_distance:

  default `100`. An integer scalar. The distance within which the
  service area polygon extends from the network.

- trim_polygon_distance_units:

  default `"meters"`. A scalar character. Units for
  `trim_polygon_distance`. One of `"meters"`, `"kilometers"`, `"feet"`,
  `"miles"`, `"nautical_miles"`, `"yards"`.

- point_barriers:

  default `NULL`. An `sf` or `sfc` object of point geometries
  representing barriers to restrict or add cost to travel.

- line_barriers:

  default `NULL`. An `sf` or `sfc` object of line geometries
  representing barriers to restrict or add cost to travel.

- polygon_barriers:

  Polygon barriers as `sf` or `sfc` object. Default: `NULL`.

- ignore_invalid_locations:

  Logical. Whether to ignore invalid locations. Default: `TRUE`.

- output_geometry_precision:

  default `10`. A numeric scalar. Simplification tolerance applied to
  output geometry.

- output_geometry_precision_units:

  default `"meters"`. A scalar character. Units for
  `output_geometry_precision`. Same valid values as
  `trim_polygon_distance_units`.

- crs:

  default `4326`. The coordinate reference system of the output
  geometries. Passed to
  [`arcgisutils::as_spatial_reference()`](https://rdrr.io/pkg/arcgisutils/man/gp_params.html).

- token:

  Authorization token. Default:
  [`arcgisutils::arc_token()`](https://rdrr.io/pkg/arcgisutils/man/token.html).

## Value

A list containing the service area results.

## References

[API
Reference](https://developers.arcgis.com/rest/routing/serviceArea-service-direct/)

## See also

Other direct:
[`find_closest_facilities()`](http://r.esri.com/arcgisrouting/reference/find_closest_facilities.md),
[`find_routes()`](http://r.esri.com/arcgisrouting/reference/find_routes.md),
[`od_cost_matrix()`](http://r.esri.com/arcgisrouting/reference/od_cost_matrix.md),
[`route_vehicles()`](http://r.esri.com/arcgisrouting/reference/route_vehicles.md),
[`snap_to_roads()`](http://r.esri.com/arcgisrouting/reference/snap_to_roads.md)

Other service area:
[`download_service_area_results()`](http://r.esri.com/arcgisrouting/reference/download_service_area_results.md),
[`find_service_areas_job()`](http://r.esri.com/arcgisrouting/reference/find_service_areas_job.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# This example is not executed since it requires a network connection
# to ArcGIS Online and a valid authentication token
library(sf)
library(arcgisutils)
set_arc_token(auth_user())

facility <- st_sfc(st_point(c(-122.253, 37.757)), crs = 4326)
find_service_areas(facility)
} # }
```
