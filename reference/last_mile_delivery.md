# Solve Last Mile Delivery (Async)

Submits an asynchronous geoprocessing job to solve a last-mile delivery
problem using the ArcGIS `/SolveLastMileDelivery` GP service. Last Mile
Delivery is a specialized vehicle routing problem that creates
geographically clustered delivery routes to minimize fleet operating
costs.

## Usage

``` r
last_mile_delivery(
  orders,
  depots,
  routes = NULL,
  order_specialties = NULL,
  route_specialties = NULL,
  zones = NULL,
  travel_mode = NULL,
  analysis_region = NULL,
  ignore_network_location_fields = FALSE,
  ignore_invalid_order_locations = FALSE,
  earliest_route_start_date = NULL,
  earliest_route_start_time = NULL,
  time_zone_usage_for_time_fields = NULL,
  max_route_total_time = NULL,
  sequence_gap = NULL,
  time_units = NULL,
  distance_units = NULL,
  route_shape = NULL,
  populate_directions = FALSE,
  directions_language = NULL,
  save_route_data = FALSE,
  save_output_network_analysis_layer = FALSE,
  output_format = "feature_set",
  point_barriers = NULL,
  line_barriers = NULL,
  polygon_barriers = NULL,
  token = arcgisutils::arc_token()
)
```

## Arguments

- orders:

  An `sf` object containing point geometries representing delivery and
  pickup locations.

- depots:

  An `sf` or `sfc` object containing point geometries representing depot
  locations.

- routes:

  A `data.frame` describing vehicle and driver characteristics. Default:
  `NULL`.

- order_specialties:

  A `data.frame` mapping orders to the specialties they require, with
  columns `order_name` and `specialty_name`. Default: `NULL`.

- route_specialties:

  A `data.frame` mapping routes to the specialties they support, with
  columns `route_name` and `specialty_name`. Default: `NULL`.

- zones:

  An `sf` or `sfc` polygon object delineating work territories, each
  carrying a `name` attribute. Default: `NULL`.

- travel_mode:

  Character. The name or ID of the travel mode to use. See
  [`get_travel_modes()`](http://r.esri.com/arcgisrouting/reference/get_travel_modes.md)
  for available options. Default: `NULL`.

- analysis_region:

  Character. Region for the analysis. One of: `"europe"`, `"japan"`,
  `"korea"`, `"middle_east_and_africa"`, `"north_america"`,
  `"south_america"`, `"south_asia"`, `"thailand"`. Default: `NULL`
  (auto-detected).

- ignore_network_location_fields:

  Logical. Whether network location fields on the inputs are ignored.
  Default: `FALSE`.

- ignore_invalid_order_locations:

  Logical. Whether to ignore invalid orders instead of failing. Default:
  `FALSE`.

- earliest_route_start_date:

  Date. Default earliest start date applied to routes whose
  `earliest_start_date` attribute is missing. Default: `NULL`.

- earliest_route_start_time:

  An `hms` or character `"hh:mm:ss"` value giving the default earliest
  start time applied to routes whose `earliest_start_time` attribute is
  missing. Default: `NULL`.

- time_zone_usage_for_time_fields:

  Character. Time zone used for all date-time input fields. One of:
  `"geo_local"`, `"utc"`. Default: `NULL` (API default: `"geo_local"`).

- max_route_total_time:

  Numeric. Default maximum allowed total time for each route, applied to
  routes whose `max_total_time` attribute is missing. Interpreted in
  `time_units`. Default: `NULL`.

- sequence_gap:

  Integer. Gap in numerical values to leave in the `Sequence` attribute
  between adjacent orders when the analysis is solved. Default: `NULL`
  (API default: `1`).

- time_units:

  Character. Units for all time-based attribute values. One of:
  `"seconds"`, `"minutes"`, `"hours"`, `"days"`. Default: `NULL` (API
  default: `"minutes"`).

- distance_units:

  Character. Units for all distance-based attribute values. One of:
  `"miles"`, `"kilometers"`, `"meters"`, `"feet"`, `"yards"`,
  `"nautical_miles"`. Default: `NULL` (API default: `"miles"`).

- route_shape:

  Character. Geometry type for output route lines. One of:
  `"true_shape_with_measures"`, `"straight_line"`, `"none"`. Default:
  `NULL` (API default: `"straight_line"`).

- populate_directions:

  Logical. Whether to generate driving directions for each route.
  Default: `FALSE`.

- directions_language:

  Character. Language code for directions (e.g. `"en"`). Default:
  `NULL`.

- save_route_data:

  Logical. Whether to save results as a `.zip` file geodatabase.
  Default: `FALSE`.

- save_output_network_analysis_layer:

  Logical. Whether the analysis settings are saved as a network analysis
  layer package file. Default: `FALSE`.

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

A `last_mile_delivery_job` R6 object inheriting from
[`arcgisutils::arc_gp_job`](https://rdrr.io/pkg/arcgisutils/man/gp_job.html).
Call `$start()` to submit and `$results` to retrieve output.

## References

[API
Reference](https://developers.arcgis.com/rest/routing/last-mile-delivery-service/)

## See also

Other async:
[`download_od_results()`](http://r.esri.com/arcgisrouting/reference/download_od_results.md),
[`download_service_area_results()`](http://r.esri.com/arcgisrouting/reference/download_service_area_results.md),
[`find_closest_facilities_job()`](http://r.esri.com/arcgisrouting/reference/find_closest_facilities_job.md),
[`find_routes_job()`](http://r.esri.com/arcgisrouting/reference/find_routes_job.md),
[`find_service_areas_job()`](http://r.esri.com/arcgisrouting/reference/find_service_areas_job.md),
[`location_allocation_job()`](http://r.esri.com/arcgisrouting/reference/location_allocation_job.md),
[`od_cost_matrix_job()`](http://r.esri.com/arcgisrouting/reference/od_cost_matrix_job.md),
[`route_vehicles_job()`](http://r.esri.com/arcgisrouting/reference/route_vehicles_job.md)

Other vrp:
[`route_vehicles()`](http://r.esri.com/arcgisrouting/reference/route_vehicles.md),
[`route_vehicles_job()`](http://r.esri.com/arcgisrouting/reference/route_vehicles_job.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# This example is not executed since it requires a network connection
# to ArcGIS Online and a valid authentication token
library(sf)
library(arcgisutils)
set_arc_token(auth_user())

orders <- st_as_sf(
  data.frame(
    name = c("Order 1", "Order 2"),
    service_time = c(5, 5),
    time_window_start = as.POSIXct(
      c(NA, 1706860800),
      origin = "1970-01-01",
      tz = "UTC"
    ),
    time_window_end = as.POSIXct(
      c(1706868000, 1706868000),
      origin = "1970-01-01",
      tz = "UTC"
    ),
    max_violation_time = c(0, 30),
    delivery_quantity_1 = c(2000, 1500),
    delivery_quantity_2 = c(100, 75),
    x = c(-117, -117.5),
    y = c(34, 34.5)
  ),
  coords = c("x", "y"),
  crs = 4326
)

depots <- st_as_sf(
  data.frame(name = "Depot 1", x = -117.2, y = 34.2),
  coords = c("x", "y"),
  crs = 4326
)

routes <- data.frame(
  name = c("Truck 1", "Truck 2"),
  start_depot_name = c("Depot 1", "Depot 1"),
  end_depot_name = c("Depot 1", "Depot 1"),
  earliest_start_time = c("6:00:00", "6:00:00"),
  capacity_1 = c(40000, 30000),
  capacity_2 = c(2000, 2500),
  cost_per_unit_time = c(0.5, 0.5),
  cost_per_unit_distance = c(1.5, 1.5)
)

order_specialties <- data.frame(
  order_name = c("Order 1", "Order 2"),
  specialty_name = c("Refrigerated", "Hazmat")
)

route_specialties <- data.frame(
  route_name = c("Truck 1", "Truck 2"),
  specialty_name = c("Refrigerated", "Hazmat")
)

zone1 <- st_polygon(list(rbind(
  c(-97.0634, 32.8442),
  c(-97.0554, 32.84),
  c(-97.0558, 32.8327),
  c(-97.0638, 32.83),
  c(-97.0634, 32.8442)
)))

zone2 <- st_multipolygon(list(
  list(rbind(
    c(-97.0803, 32.8235),
    c(-97.0776, 32.8277),
    c(-97.074, 32.8254),
    c(-97.0767, 32.8227),
    c(-97.0803, 32.8235)
  )),
  list(rbind(
    c(-97.0871, 32.8311),
    c(-97.0831, 32.8292),
    c(-97.0853, 32.8259),
    c(-97.0892, 32.8279),
    c(-97.0871, 32.8311)
  ))
))

zones <- st_cast(
  st_sf(
    name = c("Zone 1", "Zone 2"),
    geometry = st_sfc(zone1, zone2, crs = 4326)
  ),
  "MULTIPOLYGON"
)

job <- last_mile_delivery(
  orders = orders,
  depots = depots,
  routes = routes,
  order_specialties = order_specialties,
  route_specialties = route_specialties,
  zones = zones,
  earliest_route_start_date = as.Date("2024-02-02"),
  max_route_total_time = 480,
  sequence_gap = 3,
  time_units = "minutes",
  route_shape = "true_shape_with_measures",
  populate_directions = TRUE
)

job$start()
job$results
} # }
```
