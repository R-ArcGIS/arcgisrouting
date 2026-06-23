# Route Vehicles

Solves a vehicle routing problem (VRP) to find the most effective routes
for a fleet of vehicles visiting a set of orders.

## Usage

``` r
route_vehicles(
  orders,
  depots,
  routes = NULL,
  travel_mode = NULL,
  locate_settings = NULL,
  analysis_region = NULL,
  time_zone_usage_for_time_fields = NULL,
  default_date = NULL,
  breaks = NULL,
  time_units = NULL,
  distance_units = NULL,
  time_window_factor = NULL,
  spatially_cluster_routes = TRUE,
  route_zones = NULL,
  route_renewals = NULL,
  order_pairs = NULL,
  excess_transit_factor = NULL,
  uturn_policy = NULL,
  point_barriers = NULL,
  line_barriers = NULL,
  polygon_barriers = NULL,
  use_hierarchy_in_analysis = TRUE,
  restrictions = NULL,
  attribute_parameter_values = NULL,
  time_impedance = NULL,
  distance_impedance = NULL,
  impedance = NULL,
  populate_route_lines = TRUE,
  route_line_simplification_tolerance = NULL,
  populate_directions = FALSE,
  directions_language = NULL,
  directions_style_name = NULL,
  save_route_data = FALSE,
  save_output_layer = FALSE,
  populate_stop_shapes = FALSE,
  ignore_invalid_order_locations = FALSE,
  token = arcgisutils::arc_token()
)
```

## Arguments

- orders:

  an `sf` or `sfc` object containing point geometries representing
  locations to visit.

- depots:

  an `sf` or `sfc` object containing point geometries representing depot
  locations.

- routes:

  default `NULL`. A `data.frame` describing vehicle and driver
  characteristics.

- travel_mode:

  Character. The name of the travel mode to use. See
  [`get_travel_modes()`](http://r.esri.com/arcgisrouting/reference/get_travel_modes.md)
  for available options. Default: `NULL`.

- locate_settings:

  default `NULL`. A list controlling how inputs are located on the
  network.

- analysis_region:

  default `NULL`. A scalar character. One of `"europe"`, `"japan"`,
  `"korea"`, `"middle_east_and_africa"`, `"north_america"`,
  `"south_america"`, `"south_asia"`, `"thailand"`. Speeds up analysis
  when specified.

- time_zone_usage_for_time_fields:

  default `NULL`. A scalar character. One of `"geo_local"` or `"utc"`.
  Time zone used for all date-time input fields.

- default_date:

  default `NULL`. A `POSIXt` or character scalar. The date on which all
  routes start. Only the date portion is used.

- breaks:

  default `NULL`. A `data.frame` of rest period definitions for routes.

- time_units:

  default `NULL`. A scalar character. One of `"seconds"`, `"minutes"`,
  `"hours"`, `"days"`. Units for all time-based attribute values.

- distance_units:

  default `NULL`. A scalar character. One of `"miles"`, `"kilometers"`,
  `"meters"`, `"feet"`, `"yards"`, `"nautical_miles"`. Units for all
  distance-based attribute values.

- time_window_factor:

  default `NULL`. A scalar character. One of `"low"`, `"medium"`,
  `"high"`. Importance of honoring time windows.

- spatially_cluster_routes:

  default `TRUE`. A logical scalar. Whether orders assigned to a route
  are spatially clustered.

- route_zones:

  default `NULL`. An `sf` or `sfc` polygon object delineating work
  territories for routes.

- route_renewals:

  default `NULL`. A `data.frame` of intermediate depot renewal locations
  for routes.

- order_pairs:

  default `NULL`. A `data.frame` pairing pickup and delivery orders.

- excess_transit_factor:

  default `NULL`. A scalar character. One of `"low"`, `"medium"`,
  `"high"`. Importance of reducing excess transit time for order pairs.

- uturn_policy:

  default `NULL`. A scalar character. One of `"allow_uturn"`,
  `"allow_dead_ends_and_intersections_only"`, `"allow_dead_ends_only"`,
  `"no_uturns"`.

- point_barriers:

  default `NULL`. An `sf` or `sfc` object of point geometries
  representing barriers to restrict or add cost to travel.

- line_barriers:

  default `NULL`. An `sf` or `sfc` object of line geometries
  representing barriers to restrict or add cost to travel.

- polygon_barriers:

  Polygon barriers as `sf` or `sfc` object. Default: `NULL`.

- use_hierarchy_in_analysis:

  default `TRUE`. A logical scalar. Whether to use the street network
  hierarchy when finding routes.

- restrictions:

  Character vector. Restriction names to honor. Default: `NULL`.

- attribute_parameter_values:

  default `NULL`. A list of objects providing additional values required
  by an attribute or restriction.

- time_impedance:

  default `NULL`. A scalar character. Time-based impedance. One of
  `"travel_time"`, `"minutes"`, `"walk_time"`, `"truck_minutes"`,
  `"truck_travel_time"`.

- distance_impedance:

  default `NULL`. A scalar character. Distance-based impedance. One of
  `"miles"`, `"kilometers"`.

- impedance:

  default `NULL`. A scalar character. The impedance type. One of
  `"travel_time"`, `"minutes"`, `"truck_travel_time"`,
  `"truck_minutes"`, `"walk_time"`, `"miles"`, `"kilometers"`.

- populate_route_lines:

  default `TRUE`. A logical scalar. Whether output routes include the
  exact street shape.

- route_line_simplification_tolerance:

  default `NULL`. A numeric scalar. Simplification tolerance for route
  geometry.

- populate_directions:

  default `FALSE`. A logical scalar. Whether to generate driving
  directions for each route.

- directions_language:

  default `NULL`. A scalar character. Language code for directions (e.g.
  `"en"`).

- directions_style_name:

  default `NULL`. A scalar character. One of `"desktop"`,
  `"navigation"`, `"campus"`.

- save_route_data:

  default `FALSE`. A logical scalar. Whether to save results as a `.zip`
  file geodatabase.

- save_output_layer:

  default `FALSE`. A logical scalar. Whether to save the analysis as a
  network analysis layer package.

- populate_stop_shapes:

  default `FALSE`. A logical scalar. Whether output stops include point
  geometries.

- ignore_invalid_order_locations:

  default `FALSE`. A logical scalar. Whether to ignore invalid orders
  instead of failing.

- token:

  Authorization token. Default:
  [`arcgisutils::arc_token()`](https://rdrr.io/pkg/arcgisutils/man/token.html).

## Value

A named list:

- `unassigned_stops`: orders that could not be assigned to any route

- `stops`: assigned stop features with route and sequence information

- `routes`: route features with geometry and cost attributes

- `directions`: driving directions (only when
  `populate_directions = TRUE`)

- `solve_succeeded`: logical indicating whether the solve completed

- `usage_cost`: list with `numObjects` and `credits` used

- `messages`: status and warning messages from the service

## References

[API
Reference](https://developers.arcgis.com/rest/routing/vrp-service-direct/)

## See also

Other direct:
[`find_closest_facilities()`](http://r.esri.com/arcgisrouting/reference/find_closest_facilities.md),
[`find_routes()`](http://r.esri.com/arcgisrouting/reference/find_routes.md),
[`find_service_areas()`](http://r.esri.com/arcgisrouting/reference/find_service_areas.md),
[`od_cost_matrix()`](http://r.esri.com/arcgisrouting/reference/od_cost_matrix.md),
[`snap_to_roads()`](http://r.esri.com/arcgisrouting/reference/snap_to_roads.md)

Other vrp:
[`last_mile_delivery()`](http://r.esri.com/arcgisrouting/reference/last_mile_delivery.md),
[`route_vehicles_job()`](http://r.esri.com/arcgisrouting/reference/route_vehicles_job.md)

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

routes <- data.frame(
  name = "Truck1",
  start_depot_name = "Depot1",
  end_depot_name = "Depot1",
  capacities = "40000"
)

result <- route_vehicles(orders, depots, routes)
} # }
```
