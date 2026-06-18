# Generate Service Areas (Async)

Submits an asynchronous geoprocessing job to generate service areas
around facilities using the ArcGIS `/GenerateServiceAreas` GP service.

## Usage

``` r
solve_service_areas_job(
  facilities,
  break_values = NULL,
  break_units = NULL,
  travel_mode = NULL,
  travel_direction = NULL,
  time_of_day = NULL,
  use_hierarchy = NULL,
  uturn_at_junctions = NULL,
  polygons_for_multiple_facilities = NULL,
  polygon_overlap_type = NULL,
  detailed_polygons = NULL,
  polygon_trim_distance = NULL,
  polygon_simplification_tolerance = NULL,
  polygon_detail = NULL,
  output_type = NULL,
  analysis_region = NULL,
  restrictions = NULL,
  impedance = NULL,
  time_impedance = NULL,
  distance_impedance = NULL,
  ignore_invalid_locations = TRUE,
  output_format = "feature_set",
  point_barriers = NULL,
  line_barriers = NULL,
  polygon_barriers = NULL,
  token = arcgisutils::arc_token()
)
```

## Arguments

- facilities:

  An `sf` or `sfc` object containing point geometries representing the
  facilities around which service areas are generated.

- break_values:

  Numeric vector. Service area sizes. Units are determined by
  `break_units`. Default: `NULL` (API default: `5 10 15`).

- break_units:

  Character. Units for `break_values`. One of: `"meters"`,
  `"kilometers"`, `"feet"`, `"yards"`, `"miles"`, `"nautical_miles"`,
  `"seconds"`, `"minutes"`, `"hours"`, `"days"`. Default: `NULL` (API
  default: `"minutes"`).

- travel_mode:

  Character. The name or ID of the travel mode to use. See
  [`get_travel_modes()`](http://r.esri.com/arcgisrouting/reference/get_travel_modes.md)
  for available options. Default: `NULL`.

- travel_direction:

  Character. Direction of travel relative to facilities. One of:
  `"away"`, `"towards"`. Default: `NULL` (API default: `"away"`).

- time_of_day:

  POSIXct. The departure time for the routes. When `NULL`, static
  average speeds are used. Default: `NULL`.

- use_hierarchy:

  Logical. Whether to use the street hierarchy when finding routes.
  Default: `NULL` (API default: `TRUE`).

- uturn_at_junctions:

  Character. U-turn policy at junctions. One of: `"allow_backtrack"`,
  `"deadend_intersection"`, `"deadend"`, `"no_backtrack"`. Default:
  `NULL`.

- polygons_for_multiple_facilities:

  Character. How service area polygons from multiple facilities are
  generated. One of: `"overlapping"`, `"not_overlapping"`, `"merge"`.
  Default: `NULL` (API default: `"overlapping"`).

- polygon_overlap_type:

  Character. Whether polygons are rings or disks. One of: `"rings"`,
  `"disks"`. Default: `NULL` (API default: `"rings"`).

- detailed_polygons:

  Logical. Generate detailed polygons. Default: `NULL` (API default:
  `FALSE`).

- polygon_trim_distance:

  List with elements `distance` (numeric) and `units` (character). Trim
  polygons to within this distance of the network. Default: `NULL`.

- polygon_simplification_tolerance:

  List with elements `distance` (numeric) and `units` (character).
  Simplification tolerance for output polygons. Default: `NULL`.

- polygon_detail:

  Character. Level of detail for output polygons. One of: `"standard"`,
  `"generalized"`, `"high"`. Default: `NULL` (API default:
  `"standard"`).

- output_type:

  Character. Type of output to generate. One of: `"polygons"`,
  `"lines"`, `"polygons_and_lines"`. Default: `NULL` (API default:
  `"polygons"`).

- analysis_region:

  Character. Region for the analysis. One of: `"europe"`, `"japan"`,
  `"korea"`, `"middle_east_and_africa"`, `"north_america"`,
  `"south_america"`, `"south_asia"`, `"thailand"`. Default: `NULL`
  (auto-detected).

- restrictions:

  Character vector. Restriction names to apply. Default: `NULL`.

- impedance:

  Character. Impedance type. One of: `"travel_time"`, `"minutes"`,
  `"truck_travel_time"`, `"truck_minutes"`, `"walk_time"`, `"miles"`,
  `"kilometers"`. Default: `NULL`.

- time_impedance:

  Character. Time-based impedance. One of: `"minutes"`, `"travel_time"`,
  `"walk_time"`, `"truck_minutes"`, `"truck_travel_time"`. Default:
  `NULL`.

- distance_impedance:

  Character. Distance-based impedance. One of: `"miles"`,
  `"kilometers"`. Default: `NULL`.

- ignore_invalid_locations:

  Logical. Whether to ignore invalid input locations. Default: `TRUE`.

- output_format:

  Character. Format for output features. One of: `"feature_set"`,
  `"json_file"`, `"geojson_file"`. Default: `"feature_set"`.

- point_barriers:

  Point barriers as `sf` or `sfc` object. Default: `NULL`.

- line_barriers:

  Line barriers as `sf` or `sfc` object. Default: `NULL`.

- polygon_barriers:

  Polygon barriers as `sf` or `sfc` object. Default: `NULL`.

- token:

  Authorization token. Default:
  [`arcgisutils::arc_token()`](https://rdrr.io/pkg/arcgisutils/man/token.html).

## Value

A `solve_service_areas_job` R6 object inheriting from
[`arcgisutils::arc_gp_job`](https://rdrr.io/pkg/arcgisutils/man/gp_job.html).
Call `$start()` to submit and `$results` to retrieve output.

## References

[API
Reference](https://developers.arcgis.com/rest/routing/serviceArea-service-job/)

## See also

Other async:
[`download_od_results()`](http://r.esri.com/arcgisrouting/reference/download_od_results.md),
[`download_service_area_results()`](http://r.esri.com/arcgisrouting/reference/download_service_area_results.md),
[`find_closest_facilities_job()`](http://r.esri.com/arcgisrouting/reference/find_closest_facilities_job.md),
[`find_routes_job()`](http://r.esri.com/arcgisrouting/reference/find_routes_job.md),
[`last_mile_delivery()`](http://r.esri.com/arcgisrouting/reference/last_mile_delivery.md),
[`location_allocation_job()`](http://r.esri.com/arcgisrouting/reference/location_allocation_job.md),
[`od_cost_matrix_job()`](http://r.esri.com/arcgisrouting/reference/od_cost_matrix_job.md),
[`solve_vrp_job()`](http://r.esri.com/arcgisrouting/reference/solve_vrp_job.md)

Other service area:
[`download_service_area_results()`](http://r.esri.com/arcgisrouting/reference/download_service_area_results.md),
[`find_service_areas()`](http://r.esri.com/arcgisrouting/reference/find_service_areas.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(sf)
library(arcgisutils)
set_arc_token(auth_user())

facilities <- st_sfc(
  st_point(c(-122.4194, 37.7749)),
  st_point(c(-122.0312, 37.3318)),
  crs = 4326
)

job <- solve_service_areas_job(facilities, break_values = c(5, 10, 15))
job$start()
result <- job$results
} # }
```
