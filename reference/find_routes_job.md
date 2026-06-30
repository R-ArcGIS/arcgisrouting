# Find Routes (Async)

Submits an asynchronous geoprocessing job to find the best routes
between stops using the ArcGIS `/FindRoutes` GP service.

## Usage

``` r
find_routes_job(
  stops,
  travel_mode = NULL,
  measurement_units = NULL,
  analysis_region = NULL,
  reorder_stops = FALSE,
  preserve_terminal_stops = NULL,
  return_to_start = NULL,
  use_time_windows = NULL,
  time_of_day = NULL,
  uturn_at_junctions = NULL,
  use_hierarchy = NULL,
  restrictions = NULL,
  impedance = NULL,
  time_impedance = NULL,
  distance_impedance = NULL,
  route_shape = NULL,
  populate_route_edges = NULL,
  populate_directions = NULL,
  directions_language = NULL,
  directions_distance_units = NULL,
  directions_style_name = NULL,
  output_format = "feature_set",
  ignore_invalid_locations = TRUE,
  point_barriers = NULL,
  line_barriers = NULL,
  polygon_barriers = NULL,
  token = arcgisutils::arc_token()
)
```

## Arguments

- stops:

  An `sf` or `sfc` object containing point geometries. Recognized
  attribute columns such as `route_name`, `time_window_start`, and
  `time_window_end` are used when present.

- travel_mode:

  Character. The name or ID of the travel mode to use. See
  [`get_travel_modes()`](http://r.esri.com/arcgisrouting/reference/get_travel_modes.md)
  for available options. Default: `NULL`.

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

- reorder_stops:

  Logical. Reorder stops to find the optimal route (TSP). Default:
  `FALSE`.

- preserve_terminal_stops:

  Character. Which terminal stops to preserve when
  `reorder_stops = TRUE`. One of: `"first"`, `"last"`,
  `"first_and_last"`, `"none"`. Default: `NULL` (API default:
  `"first"`).

- return_to_start:

  Logical. Whether the route should return to its starting location.
  Default: `NULL` (API default: `TRUE`).

- use_time_windows:

  Logical. Whether to honour time windows on stops. Default: `NULL`
  (auto-detected from `stops` attributes).

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

- route_shape:

  Character. Shape of the output route features. One of: `"true_shape"`,
  `"true_shape_with_measures"`, `"straight_line"`, `"none"`. Default:
  `NULL` (API default: `"true_shape"`).

- populate_route_edges:

  Logical. Generate edges for each route. Default: `NULL` (API default:
  `FALSE`).

- populate_directions:

  Logical. Generate driving directions. Default: `NULL` (API default:
  `FALSE`).

- directions_language:

  Character. Language code for directions. Default: `NULL` (API default:
  `"en"`).

- directions_distance_units:

  Character. Units for distances in directions. One of: `"feet"`,
  `"kilometers"`, `"meters"`, `"miles"`, `"nautical_miles"`, `"yards"`.
  Default: `NULL`.

- directions_style_name:

  Character. Formatting style for directions. One of: `"desktop"`,
  `"navigation"`. Default: `NULL`.

- output_format:

  Character. Format for output features. One of: `"feature_set"`,
  `"json_file"`, `"geojson_file"`. Default: `"feature_set"`.

- ignore_invalid_locations:

  Logical. Whether to ignore invalid input locations. Default: `TRUE`.

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

A `find_routes_job` R6 object inheriting from
[`arcgisutils::arc_gp_job`](https://rdrr.io/pkg/arcgisutils/man/gp_job.html).
Call `$start()` to submit and `$results` to retrieve output.

## References

[API
Reference](https://developers.arcgis.com/rest/routing/route-service-job/)

## See also

Other async:
[`download_od_results()`](http://r.esri.com/arcgisrouting/reference/download_od_results.md),
[`download_service_area_results()`](http://r.esri.com/arcgisrouting/reference/download_service_area_results.md),
[`find_closest_facilities_job()`](http://r.esri.com/arcgisrouting/reference/find_closest_facilities_job.md),
[`find_service_areas_job()`](http://r.esri.com/arcgisrouting/reference/find_service_areas_job.md),
[`last_mile_delivery()`](http://r.esri.com/arcgisrouting/reference/last_mile_delivery.md),
[`location_allocation_job()`](http://r.esri.com/arcgisrouting/reference/location_allocation_job.md),
[`od_cost_matrix_job()`](http://r.esri.com/arcgisrouting/reference/od_cost_matrix_job.md),
[`route_vehicles_job()`](http://r.esri.com/arcgisrouting/reference/route_vehicles_job.md)

Other routing:
[`find_routes()`](http://r.esri.com/arcgisrouting/reference/find_routes.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# This example is not executed since it requires a network connection
# to ArcGIS Online and a valid authentication token
library(sf)
library(arcgisutils)
set_arc_token(auth_user())

stops <- st_sf(
  name = c("Stop 1", "Stop 2", "Stop 3"),
  geometry = st_sfc(
    st_point(c(145.066, -37.865)),
    st_point(c(145.105, -37.819)),
    st_point(c(145.120, -37.800)),
    crs = 4326
  )
)

job <- find_routes_job(stops)
job$start()
result <- job$results
} # }
```
