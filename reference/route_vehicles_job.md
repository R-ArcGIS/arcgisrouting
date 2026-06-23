# Solve Vehicle Routing Problem (Async)

Submits an asynchronous geoprocessing job to solve a vehicle routing
problem using the ArcGIS `/SolveVehicleRoutingProblem` GP service.

## Usage

``` r
route_vehicles_job(
  orders,
  depots,
  routes = NULL,
  breaks = NULL,
  travel_mode = NULL,
  analysis_region = NULL,
  time_zone_usage_for_time_fields = NULL,
  default_date = NULL,
  time_units = NULL,
  distance_units = NULL,
  time_window_factor = NULL,
  spatially_cluster_routes = TRUE,
  route_zones = NULL,
  route_renewals = NULL,
  order_pairs = NULL,
  excess_transit_factor = NULL,
  uturn_policy = NULL,
  restrictions = NULL,
  impedance = NULL,
  time_impedance = NULL,
  distance_impedance = NULL,
  use_hierarchy_in_analysis = TRUE,
  populate_route_lines = TRUE,
  route_line_simplification_tolerance = NULL,
  populate_directions = FALSE,
  directions_language = NULL,
  directions_style_name = NULL,
  save_route_data = FALSE,
  save_output_layer = FALSE,
  populate_stop_shapes = FALSE,
  output_format = "feature_set",
  ignore_invalid_order_locations = FALSE,
  point_barriers = NULL,
  line_barriers = NULL,
  polygon_barriers = NULL,
  token = arcgisutils::arc_token()
)
```

## Arguments

- orders:

  An `sf` or `sfc` object containing point geometries representing
  locations to visit.

- depots:

  An `sf` or `sfc` object containing point geometries representing depot
  locations.

- routes:

  A `data.frame` describing vehicle and driver characteristics. Default:
  `NULL`.

- breaks:

  A `data.frame` of rest period definitions for routes. Default: `NULL`.

- travel_mode:

  Character. The name or ID of the travel mode to use. See
  [`get_travel_modes()`](http://r.esri.com/arcgisrouting/reference/get_travel_modes.md)
  for available options. Default: `NULL`.

- analysis_region:

  Character. Region for the analysis. One of: `"europe"`, `"japan"`,
  `"korea"`, `"middle_east_and_africa"`, `"north_america"`,
  `"south_america"`, `"south_asia"`, `"thailand"`. Default: `NULL`
  (auto-detected).

- time_zone_usage_for_time_fields:

  Character. Time zone used for all date-time input fields. One of:
  `"geo_local"`, `"utc"`. Default: `NULL` (API default: `"geo_local"`).

- default_date:

  POSIXct. The date on which all routes start. Only the date portion is
  used. Default: `NULL`.

- time_units:

  Character. Units for all time-based attribute values. One of:
  `"seconds"`, `"minutes"`, `"hours"`, `"days"`. Default: `NULL` (API
  default: `"minutes"`).

- distance_units:

  Character. Units for all distance-based attribute values. One of:
  `"miles"`, `"kilometers"`, `"meters"`, `"feet"`, `"yards"`,
  `"nautical_miles"`. Default: `NULL` (API default: `"miles"`).

- time_window_factor:

  Character. Importance of honoring time windows. One of: `"low"`,
  `"medium"`, `"high"`. Default: `NULL` (API default: `"medium"`).

- spatially_cluster_routes:

  Logical. Whether orders assigned to a route are spatially clustered.
  Default: `TRUE`.

- route_zones:

  An `sf` or `sfc` polygon object delineating work territories for
  routes. Default: `NULL`.

- route_renewals:

  A `data.frame` of intermediate depot renewal locations. Default:
  `NULL`.

- order_pairs:

  A `data.frame` pairing pickup and delivery orders. Default: `NULL`.

- excess_transit_factor:

  Character. Importance of reducing excess transit time for order pairs.
  One of: `"low"`, `"medium"`, `"high"`. Default: `NULL` (API default:
  `"medium"`).

- uturn_policy:

  Character. U-turn policy at junctions. One of: `"allow_uturn"`,
  `"allow_dead_ends_and_intersections_only"`, `"allow_dead_ends_only"`,
  `"no_uturns"`. Default: `NULL`.

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

- use_hierarchy_in_analysis:

  Logical. Whether to use the street network hierarchy when finding
  routes. Default: `TRUE`.

- populate_route_lines:

  Logical. Whether output routes include the exact street shape.
  Default: `TRUE`.

- route_line_simplification_tolerance:

  Numeric. Simplification tolerance for route geometry. Default: `NULL`.

- populate_directions:

  Logical. Whether to generate driving directions for each route.
  Default: `FALSE`.

- directions_language:

  Character. Language code for directions (e.g. `"en"`). Default:
  `NULL`.

- directions_style_name:

  Character. Formatting style for directions. One of: `"desktop"`,
  `"navigation"`, `"campus"`. Default: `NULL`.

- save_route_data:

  Logical. Whether to save results as a `.zip` file geodatabase.
  Default: `FALSE`.

- save_output_layer:

  Logical. Whether to save the analysis as a network analysis layer
  package. Default: `FALSE`.

- populate_stop_shapes:

  Logical. Whether output stops include point geometries. Default:
  `FALSE`.

- output_format:

  Character. Format for output features. One of: `"feature_set"`,
  `"json_file"`, `"geojson_file"`. Default: `"feature_set"`.

- ignore_invalid_order_locations:

  Logical. Whether to ignore invalid orders instead of failing. Default:
  `FALSE`.

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

A `route_vehicles_job` R6 object inheriting from
[`arcgisutils::arc_gp_job`](https://rdrr.io/pkg/arcgisutils/man/gp_job.html).
Call `$start()` to submit and `$results` to retrieve output.

## References

[API
Reference](https://developers.arcgis.com/rest/routing/vrp-service-job/)

## See also

Other async:
[`download_od_results()`](http://r.esri.com/arcgisrouting/reference/download_od_results.md),
[`download_service_area_results()`](http://r.esri.com/arcgisrouting/reference/download_service_area_results.md),
[`find_closest_facilities_job()`](http://r.esri.com/arcgisrouting/reference/find_closest_facilities_job.md),
[`find_routes_job()`](http://r.esri.com/arcgisrouting/reference/find_routes_job.md),
[`find_service_areas_job()`](http://r.esri.com/arcgisrouting/reference/find_service_areas_job.md),
[`last_mile_delivery()`](http://r.esri.com/arcgisrouting/reference/last_mile_delivery.md),
[`location_allocation_job()`](http://r.esri.com/arcgisrouting/reference/location_allocation_job.md),
[`od_cost_matrix_job()`](http://r.esri.com/arcgisrouting/reference/od_cost_matrix_job.md)

Other vrp:
[`last_mile_delivery()`](http://r.esri.com/arcgisrouting/reference/last_mile_delivery.md),
[`route_vehicles()`](http://r.esri.com/arcgisrouting/reference/route_vehicles.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(sf)
library(arcgisutils)
set_arc_token(auth_user())

orders <- st_sf(
  name = c("Order 1", "Order 2"),
  geometry = st_sfc(
    st_point(c(-0.1891, 51.5254)),
    st_point(c(-0.1744, 51.5353)),
    crs = 4326
  )
)

depots <- st_sf(
  name = "Depot1",
  geometry = st_sfc(st_point(c(-0.2, 51.5)), crs = 4326)
)

job <- route_vehicles_job(orders, depots)
job$start()
result <- job$results
} # }
```
