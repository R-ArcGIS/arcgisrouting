# Generate Origin-Destination Cost Matrix (Async)

Submits an asynchronous geoprocessing job that creates an
origin-destination (OD) cost matrix from multiple origins to multiple
destinations using the ArcGIS `/GenerateOriginDestinationCostMatrix` GP
service. The matrix reports the travel time or travel distance from
every origin to every destination.

## Usage

``` r
od_cost_matrix_job(
  origins,
  destinations = NULL,
  travel_mode = NULL,
  time_units = NULL,
  distance_units = NULL,
  analysis_region = NULL,
  n_dests = NULL,
  cutoff = NULL,
  time_of_day = NULL,
  uturn_at_junctions = NULL,
  use_hierarchy = NULL,
  restrictions = NULL,
  attribute_parameter_values = NULL,
  origin_destination_line_shape = NULL,
  impedance = NULL,
  time_impedance = NULL,
  distance_impedance = NULL,
  point_barriers = NULL,
  line_barriers = NULL,
  polygon_barriers = NULL,
  save_output_network_analysis_layer = NULL,
  output_format = "feature_set",
  ignore_invalid_locations = TRUE,
  token = arcgisutils::arc_token()
)
```

## Arguments

- origins:

  An `sf` or `sfc` object containing point geometries that function as
  starting points in generating the paths to destinations.

- destinations:

  An `sf` or `sfc` object containing point geometries that function as
  ending points in generating the paths from origins. Default: `NULL`.

- travel_mode:

  Character. The name or ID of the travel mode to use. See
  [`get_travel_modes()`](http://r.esri.com/arcgisrouting/reference/get_travel_modes.md)
  for available options. Default: `NULL`.

- time_units:

  Character. Units used to measure and report the total travel time
  between each origin-destination pair. One of: `"seconds"`,
  `"minutes"`, `"hours"`, `"days"`. Default: `NULL` (API default:
  `"minutes"`).

- distance_units:

  Character. Units used to measure and report the total travel distance
  between each origin-destination pair. One of: `"meters"`,
  `"kilometers"`, `"feet"`, `"yards"`, `"miles"`, `"nautical_miles"`.
  Default: `NULL` (API default: `"miles"`).

- analysis_region:

  Character. Region for the analysis. One of: `"europe"`, `"japan"`,
  `"korea"`, `"middle_east_and_africa"`, `"north_america"`,
  `"south_america"`, `"south_asia"`, `"thailand"`. Default: `NULL`
  (auto-detected).

- n_dests:

  Integer. The maximum number of destinations to find per origin.
  Default: `NULL` (API default: every destination).

- cutoff:

  Numeric. The travel time or travel distance value at which to stop
  searching for destinations from a given origin. Default: `NULL`.

- time_of_day:

  POSIXct. The departure time for the routes. When `NULL`, static
  average speeds are used. Default: `NULL`.

- uturn_at_junctions:

  Character. U-turn policy at junctions. One of: `"allow_backtrack"`,
  `"deadend_intersection"`, `"deadend"`, `"no_backtrack"`. Default:
  `NULL`.

- use_hierarchy:

  Logical. Whether to use the street hierarchy when finding routes.
  Default: `NULL` (API default: `TRUE`).

- restrictions:

  Character vector. Restriction names to apply. Default: `NULL`.

- origin_destination_line_shape:

  Character. Shape of the line feature connecting each
  origin-destination pair in the output matrix. One of:
  `"straight_line"` or `"none"`. Default: `NULL` (API default:
  `"none"`).

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

- point_barriers:

  Point barriers as `sf` or `sfc` object. Default: `NULL`.

- line_barriers:

  Line barriers as `sf` or `sfc` object. Default: `NULL`.

- polygon_barriers:

  Polygon barriers as `sf` or `sfc` object. Default: `NULL`.

- output_format:

  Character. Format for output features. One of: `"feature_set"`,
  `"json_file"`, `"geojson_file"`. Default: `"feature_set"`.

- ignore_invalid_locations:

  Logical. Whether to ignore invalid input locations. Default: `TRUE`.

- token:

  Authorization token. Default:
  [`arcgisutils::arc_token()`](https://rdrr.io/pkg/arcgisutils/man/token.html).

## Value

An `od_cost_matrix_job` R6 object inheriting from
[`arcgisutils::arc_gp_job`](https://rdrr.io/pkg/arcgisutils/man/gp_job.html).
Call `$start()` to submit and `$results` to retrieve output.

## References

[API
Reference](https://developers.arcgis.com/rest/routing/travelCostMatrix-service-job/)

## See also

Other async:
[`download_od_results()`](http://r.esri.com/arcgisrouting/reference/download_od_results.md),
[`download_service_area_results()`](http://r.esri.com/arcgisrouting/reference/download_service_area_results.md),
[`find_closest_facilities_job()`](http://r.esri.com/arcgisrouting/reference/find_closest_facilities_job.md),
[`find_routes_job()`](http://r.esri.com/arcgisrouting/reference/find_routes_job.md),
[`find_service_areas_job()`](http://r.esri.com/arcgisrouting/reference/find_service_areas_job.md),
[`last_mile_delivery()`](http://r.esri.com/arcgisrouting/reference/last_mile_delivery.md),
[`location_allocation_job()`](http://r.esri.com/arcgisrouting/reference/location_allocation_job.md),
[`route_vehicles_job()`](http://r.esri.com/arcgisrouting/reference/route_vehicles_job.md)

Other od:
[`download_od_results()`](http://r.esri.com/arcgisrouting/reference/download_od_results.md),
[`od_cost_matrix()`](http://r.esri.com/arcgisrouting/reference/od_cost_matrix.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(sf)
library(arcgisutils)
set_arc_token(auth_user())

origins <- st_sf(
  name = c("Origin 1", "Origin 2"),
  n_dests = c(2L, 3L),
  cutoff = c(120, 90),
  curb_approach = c(0L, 0L),
  geometry = st_sfc(
    st_point(c(-0.1891, 51.5254)),
    st_point(c(-0.1744, 51.5353)),
    crs = 4326
  )
)

destinations <- st_sf(
  name = c("Destination 1", "Destination 2"),
  curb_approach = c(0L, 0L),
  geometry = st_sfc(
    st_point(c(-0.1991, 51.5354)),
    st_point(c(-0.1844, 51.5458)),
    crs = 4326
  )
)

job <- od_cost_matrix_job(origins, destinations)
job$start()
result <- job$results
} # }
```
