# Snap Points to Roads

Snaps a set of GPS points to the most likely roads traveled and
optionally returns the traversed road segments as lines using the ArcGIS
`/SnapToRoads` service.

## Usage

``` r
snap_to_roads(
  points,
  travel_mode = NULL,
  point_properties = c("oid_routing_streets", "posted_speed_limit_kph",
    "posted_speed_limit_mph", "posted_speed_limit_mps", "posted_truck_speed_limit_kph",
    "posted_truck_speed_limit_mph", "posted_truck_speed_limit_mps"),
  line_properties = c("oid_routing_streets", "length_kilometers", "length_miles",
    "posted_speed_limit_kph", "posted_speed_limit_mph", "posted_speed_limit_mps",
    "posted_truck_speed_limit_kph", "posted_truck_speed_limit_mph",
    "posted_truck_speed_limit_mps"),
  analysis_region = c("europe", "japan", "korea", "middleastandafrica", "northamerica",
    "southamerica", "southasia", "thailand"),
  return_lines = TRUE,
  token = arc_token()
)
```

## Arguments

- points:

  An `sf` or `sfc` object containing the point geometries to snap to the
  road network. Recognized attribute columns such as `track_id` and
  `location_timestamp` are used when present.

- travel_mode:

  Character. The name of the travel mode to use. See
  [`get_travel_modes()`](http://r.esri.com/arcgisrouting/reference/get_travel_modes.md)
  for available options. Default: `NULL`.

- point_properties:

  Character vector. Road properties returned on the snapped points
  output. One or more of: `"oid_routing_streets"`,
  `"posted_speed_limit_kph"`, `"posted_speed_limit_mph"`,
  `"posted_speed_limit_mps"`, `"posted_truck_speed_limit_kph"`,
  `"posted_truck_speed_limit_mph"`, `"posted_truck_speed_limit_mps"`.

- line_properties:

  Character vector. Road properties returned on the output lines. One or
  more of: `"oid_routing_streets"`, `"length_kilometers"`,
  `"length_miles"`, `"posted_speed_limit_kph"`,
  `"posted_speed_limit_mph"`, `"posted_speed_limit_mps"`,
  `"posted_truck_speed_limit_kph"`, `"posted_truck_speed_limit_mph"`,
  `"posted_truck_speed_limit_mps"`. Ignored unless
  `return_lines = TRUE`.

- analysis_region:

  Character. Region in which to perform the analysis. One of:
  `"europe"`, `"japan"`, `"korea"`, `"middleastandafrica"`,
  `"northamerica"`, `"southamerica"`, `"southasia"`, `"thailand"`.
  Default: auto-detected from the location of `points`.

- return_lines:

  Logical. Whether to return output lines representing the roads
  traversed. Default: `TRUE`.

- token:

  Authorization token. Default:
  [`arcgisutils::arc_token()`](https://rdrr.io/pkg/arcgisutils/man/token.html).

## Value

A named list:

- `snapped_points`: the input points snapped to the road network

- `snap_lines`: the traversed road segments (only when
  `return_lines = TRUE`)

- `usage_cost`: list with `numObjects` and `credits` used

- `messages`: status and warning messages from the service

## References

[API
Reference](https://developers.arcgis.com/rest/routing/snap-to-roads-direct/)

## See also

Other direct:
[`find_closest_facilities()`](http://r.esri.com/arcgisrouting/reference/find_closest_facilities.md),
[`find_routes()`](http://r.esri.com/arcgisrouting/reference/find_routes.md),
[`find_service_areas()`](http://r.esri.com/arcgisrouting/reference/find_service_areas.md),
[`od_cost_matrix()`](http://r.esri.com/arcgisrouting/reference/od_cost_matrix.md),
[`route_vehicles()`](http://r.esri.com/arcgisrouting/reference/route_vehicles.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(sf)
library(arcgisutils)
set_arc_token(auth_user())

pts <- st_sfc(
  st_point(c(-122.43410, 37.80016)),
  st_point(c(-122.43460, 37.80074)),
  st_point(c(-122.43539, 37.80096)),
  crs = 4326
)

snap_to_roads(pts)
} # }
```
