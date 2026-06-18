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
  pickup locations. Use
  [`as_lmd_orders()`](http://r.esri.com/arcgisrouting/reference/as_lmd_orders.md)
  to prepare with validated attributes.

- depots:

  An `sf` or `sfc` object containing point geometries representing depot
  locations. Prepared with
  [`as_stops()`](http://r.esri.com/arcgisrouting/reference/as_stops.md).

- routes:

  A `data.frame` describing vehicle and driver characteristics. Use
  [`as_lmd_routes()`](http://r.esri.com/arcgisrouting/reference/as_lmd_routes.md)
  to prepare with validated attributes. Default: `NULL`.

- order_specialties:

  A `data.frame` mapping orders to the specialties they require, with
  columns `order_name` and `specialty_name`. Default: `NULL`.

- route_specialties:

  A `data.frame` mapping routes to the specialties they support, with
  columns `route_name` and `specialty_name`. Default: `NULL`.

- zones:

  An `sf` or `sfc` polygon object delineating work territories, each
  carrying a `name` attribute. Use
  [`as_lmd_zones()`](http://r.esri.com/arcgisrouting/reference/as_lmd_zones.md)
  to prepare. Default: `NULL`.

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
