# Solve Location-Allocation (Async)

Submits an asynchronous geoprocessing job that chooses the set of
facilities which best serve demand from surrounding areas,
simultaneously locating facilities and allocating demand points to them,
using the ArcGIS `/SolveLocationAllocation` GP service.

## Usage

``` r
location_allocation_job(
  facilities,
  demand_points,
  travel_mode = NULL,
  locate_settings = NULL,
  measurement_units = NULL,
  analysis_region = NULL,
  problem_type = NULL,
  number_of_facilities_to_find = NULL,
  default_measurement_cutoff = NULL,
  default_capacity = NULL,
  target_market_share = NULL,
  measurement_transformation_model = NULL,
  measurement_transformation_factor = NULL,
  travel_direction = NULL,
  time_of_day = NULL,
  uturn_at_junctions = NULL,
  use_hierarchy = NULL,
  restrictions = NULL,
  attribute_parameter_values = NULL,
  allocation_line_shape = NULL,
  time_impedance = NULL,
  distance_impedance = NULL,
  impedance = NULL,
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

- facilities:

  An `sf` or `sfc` object containing point geometries representing the
  locations that serve as facilities.

- demand_points:

  An `sf` or `sfc` object containing point geometries representing the
  demand points.

- travel_mode:

  Character. The name or ID of the travel mode to use. See
  [`get_travel_modes()`](http://r.esri.com/arcgisrouting/reference/get_travel_modes.md)
  for available options. Default: `NULL`.

- locate_settings:

  List. Settings that affect how input data are located, passed through
  as a JSON object. Default: `NULL`.

- measurement_units:

  Character. Units for reporting total travel time or distance. One of:
  `"meters"`, `"kilometers"`, `"feet"`, `"yards"`, `"miles"`,
  `"nautical_miles"`, `"seconds"`, `"minutes"`, `"hours"`, `"days"`.
  Default: `NULL` (API default: `"minutes"`).

- analysis_region:

  Character. Region for the analysis. One of: `"europe"`, `"japan"`,
  `"korea"`, `"middle_east_and_africa"`, `"north_america"`,
  `"south_america"`, `"south_asia"`, `"thailand"`. Default: `NULL`
  (auto-detected).

- problem_type:

  Character. Objective of the location-allocation analysis. One of:
  `"maximize_attendance"`, `"maximize_capacitated_coverage"`,
  `"maximize_coverage"`, `"maximize_market_share"`,
  `"minimize_facilities"`, `"minimize_impedance"`,
  `"target_market_share"`. Default: `NULL` (API default:
  `"minimize_impedance"`).

- number_of_facilities_to_find:

  Integer. The number of facilities the task should choose. Default:
  `NULL` (API default: `1`).

- default_measurement_cutoff:

  Numeric. The maximum travel time or distance allowed between a demand
  point and the facility to which it is allocated. Units are determined
  by `measurement_units`. Default: `NULL` (API default: no cutoff).

- default_capacity:

  Numeric. The default capacity assigned to all facilities. Only
  applicable to the `"maximize_capacitated_coverage"` problem type.
  Default: `NULL` (API default: `1`).

- target_market_share:

  Numeric. The percentage of total demand weight to capture. Only
  applicable to the `"target_market_share"` problem type. Default:
  `NULL` (API default: `10`).

- measurement_transformation_model:

  Character. Equation for transforming the network cost between
  facilities and demand points. One of: `"linear"`, `"power"`,
  `"exponential"`. Default: `NULL` (API default: `"linear"`).

- measurement_transformation_factor:

  Numeric. The impedance parameter value (lambda) for
  `measurement_transformation_model`. Ignored when the model is linear.
  Default: `NULL` (API default: `1`).

- travel_direction:

  Character. Direction in which to measure travel. One of:
  `"facility_to_demand"` or `"demand_to_facility"`. Default: `NULL` (API
  default: `"facility_to_demand"`).

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

- allocation_line_shape:

  Character. Type of line features output by the request. One of:
  `"straight_line"` or `"none"`. Default: `NULL` (API default:
  `"straight_line"`).

- time_impedance:

  Character. Time-based impedance. One of: `"minutes"`, `"travel_time"`,
  `"walk_time"`, `"truck_minutes"`, `"truck_travel_time"`. Default:
  `NULL`.

- distance_impedance:

  Character. Distance-based impedance. One of: `"miles"`,
  `"kilometers"`. Default: `NULL`.

- impedance:

  Character. Impedance type. One of: `"travel_time"`, `"minutes"`,
  `"truck_travel_time"`, `"truck_minutes"`, `"walk_time"`, `"miles"`,
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

A `location_allocation_job` R6 object inheriting from
[`arcgisutils::arc_gp_job`](https://rdrr.io/pkg/arcgisutils/man/gp_job.html).
Call `$start()` to submit and `$results` to retrieve output.

## References

[API
Reference](https://developers.arcgis.com/rest/routing/solve-location-allocation/)

## See also

Other async:
[`download_od_results()`](http://r.esri.com/arcgisrouting/reference/download_od_results.md),
[`download_service_area_results()`](http://r.esri.com/arcgisrouting/reference/download_service_area_results.md),
[`find_closest_facilities_job()`](http://r.esri.com/arcgisrouting/reference/find_closest_facilities_job.md),
[`find_routes_job()`](http://r.esri.com/arcgisrouting/reference/find_routes_job.md),
[`last_mile_delivery()`](http://r.esri.com/arcgisrouting/reference/last_mile_delivery.md),
[`od_cost_matrix_job()`](http://r.esri.com/arcgisrouting/reference/od_cost_matrix_job.md),
[`solve_service_areas_job()`](http://r.esri.com/arcgisrouting/reference/solve_service_areas_job.md),
[`solve_vrp_job()`](http://r.esri.com/arcgisrouting/reference/solve_vrp_job.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(sf)
library(arcgisutils)
set_arc_token(auth_user())

facilities <- st_sf(
  name = c("Facility A", "Facility B"),
  facility_type = c(0L, 0L),
  curb_approach = c(0L, 0L),
  geometry = st_sfc(
    st_point(c(-58.557329417999938, -34.587693706999971)),
    st_point(c(-58.460247408999976, -34.683348039999942)),
    crs = 4326
  )
)

demand_points <- st_sf(
  name = c("Household 4", "Household 3", "Household 2", "Household 1"),
  group_name = c("A", "A", NA, NA),
  weight = c(2, 2, 3, 5),
  curb_approach = c(0L, 0L, 0L, 1L),
  geometry = st_sfc(
    st_point(c(-58.664405163999959, -34.614819562999969)),
    st_point(c(-58.514499119999982, -34.496322404999944)),
    st_point(c(-58.54162497599998, -34.788996107999935)),
    st_point(c(-58.40599569799997, -34.637662387999967)),
    crs = 4326
  )
)

job <- location_allocation_job(facilities, demand_points)
job$start()
result <- job$results
} # }
```
