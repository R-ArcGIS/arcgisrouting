# Find Routes

Finds the best routes between multiple stops using the ArcGIS routing
service.

## Usage

``` r
find_routes(
  stops,
  travel_mode = NULL,
  start_time = NULL,
  find_best_sequence = FALSE,
  preserve_first_stop = TRUE,
  preserve_last_stop = TRUE,
  restrict_u_turns = NULL,
  use_hierarchy = NULL,
  impedance_attribute_name = NULL,
  accumulate_attribute_names = NULL,
  restrictions = NULL,
  barriers = NULL,
  polyline_barriers = NULL,
  polygon_barriers = NULL,
  directions_language = "en",
  directions_type = "standard",
  return_geometry = c("routes", "directions"),
  ignore_invalid_locations = TRUE,
  token = arcgisutils::arc_token()
)
```

## Arguments

- stops:

  An `sf` or `sfc` object containing point geometries representing the
  stops to visit. Use
  [`as_stops()`](http://r.esri.com/arcgisrouting/reference/as_stops.md)
  to prepare stops with attributes.

- travel_mode:

  Character. The name of the travel mode to use. See
  [`get_travel_modes()`](http://r.esri.com/arcgisrouting/reference/get_travel_modes.md)
  for available options. Default: `NULL`.

- start_time:

  POSIXct or character. The time at which travel begins. Can be `"now"`
  for current time, a POSIXct datetime, or `NULL` for static speeds.
  Default: `NULL`.

- find_best_sequence:

  Logical. Whether to reorder stops to find the optimized route.
  Default: `FALSE`.

- preserve_first_stop:

  Logical. Whether to keep the first stop fixed when reordering. Only
  applies if `find_best_sequence = TRUE`. Default: `TRUE`.

- preserve_last_stop:

  Logical. Whether to keep the last stop fixed when reordering. Only
  applies if `find_best_sequence = TRUE`. Default: `TRUE`.

- restrict_u_turns:

  Character. Specifies U-turn restrictions. One of: `"allow_backtrack"`,
  `"deadend_intersection"`, `"deadend"`, `"no_backtrack"`. Default:
  `"allow_backtrack"`.

- use_hierarchy:

  Logical. Whether to use hierarchy when finding routes. Default:
  `TRUE`.

- impedance_attribute_name:

  Character. The impedance to use. One of: `"travel_time"`, `"minutes"`,
  `"truck_travel_time"`, `"truck_minutes"`, `"walk_time"`, `"miles"`,
  `"kilometers"`. Default: `NULL`.

- accumulate_attribute_names:

  Character vector. Additional impedance values to accumulate. Default:
  `NULL`.

- restrictions:

  Character vector. Restriction names to honor. Default: `NULL`.

- barriers:

  Point barriers as `sf` or `sfc` object. Default: `NULL`.

- polyline_barriers:

  Line barriers as `sf` or `sfc` object. Default: `NULL`.

- polygon_barriers:

  Polygon barriers as `sf` or `sfc` object. Default: `NULL`.

- directions_language:

  Character. Language code for directions (e.g., `"en"`). Default:
  `"en"`.

- directions_type:

  Character. Specifies the content and verbosity of driving directions
  (`directionsOutputType` in REST API). One of: `"complete"`,
  `"complete_no_events"`, `"instructions_only"`, `"standard"`,
  `"summary_only"`, `"feature_sets"`. Default: `"standard"`.

- return_geometry:

  Character vector. Specifies which features to return in the output.
  Valid values: `"routes"`, `"directions"`, `"stops"`, `"barriers"`,
  `"polyline_barriers"`, `"polygon_barriers"`, `"traversed_edges"`,
  `"traversed_junctions"`, `"traversed_turns"`. Default:
  `c("routes", "directions")`.

- ignore_invalid_locations:

  Logical. Whether to ignore invalid locations. Default: `TRUE`.

- token:

  Authorization token. Default:
  [`arcgisutils::arc_token()`](https://rdrr.io/pkg/arcgisutils/man/token.html).

- return_directions:

  Logical. Whether to generate driving directions. Default: `TRUE`.

## Value

A list containing the routing resps. The elements returned depend on the
`return_geometry` parameter. Possible elements include:

- `routes`: Route features

- `directions`: Driving directions. Each element contains a
  `compress_geometry` column with per-maneuver segment geometry in
  ArcGIS compressed format. Use
  [`decode_compressed_geometry()`](http://r.esri.com/arcgisrouting/reference/decode_compressed_geometry.md)
  to decode these into sf geometries.

- `stops`: Stop features

- `barriers`: Barrier features

- `polyline_barriers`: Polyline barrier features

- `polygon_barriers`: Polygon barrier features

- `traversed_edges`: Traversed edge features

- `traversed_junctions`: Traversed junction features

- `traversed_turns`: Traversed turn features

- `messages`: Status and warning messages from the service

When `directions_type = "feature_sets"`, the response includes:

- `direction_points`: sf object with point features for direction
  maneuvers

- `direction_lines`: sf object with line features for route segments

## References

[API
Reference](https://developers.arcgis.com/rest/routing/route-service-direct/)

## Examples

``` r
if (FALSE) { # \dontrun{
library(sf)

# Simple route between 3 stops
stops <- st_sf(
  name = c("Start", "Middle", "End"),
  geometry = st_sfc(
    st_point(c(-122.4194, 37.7749)),  # San Francisco
    st_point(c(-122.0312, 37.3318)),  # Cupertino
    st_point(c(-121.8863, 37.3382)),  # San Jose
    crs = 4326
  )
)

resp <- find_routes(stops)

# Route with time windows for deliveries
delivery_stops <- st_sf(
  name = c("Warehouse", "Customer A", "Customer B", "Customer C"),
  time_window_start = as.POSIXct(c(
    "2024-01-15 08:00:00",
    "2024-01-15 09:00:00",
    "2024-01-15 11:00:00",
    "2024-01-15 14:00:00"
  )),
  time_window_end = as.POSIXct(c(
    "2024-01-15 08:30:00",
    "2024-01-15 10:00:00",
    "2024-01-15 13:00:00",
    "2024-01-15 16:00:00"
  )),
  geometry = st_sfc(
    st_point(c(-122.4194, 37.7749)),
    st_point(c(-122.4083, 37.7858)),
    st_point(c(-122.4313, 37.7793)),
    st_point(c(-122.4000, 37.7900)),
    crs = 4326
  )
)

delivery_route <- find_routes(
  delivery_stops,
  start_time = as.POSIXct("2024-01-15 08:00:00"),
  travel_mode = "Driving Time"
)

# Find optimal order for stops
sales_stops <- st_sf(
  name = c("Office", "Client 1", "Client 2", "Client 3", "Office"),
  geometry = st_sfc(
    st_point(c(-122.4194, 37.7749)),
    st_point(c(-122.4083, 37.7858)),
    st_point(c(-122.4313, 37.7793)),
    st_point(c(-122.4000, 37.7900)),
    st_point(c(-122.4194, 37.7749)),
    crs = 4326
  )
)

optimized_route <- find_routes(
  sales_stops,
  find_best_sequence = TRUE,
  preserve_first_stop = TRUE,
  preserve_last_stop = TRUE
)

# Route with barriers
barrier_pts <- st_sfc(
  st_point(c(-122.4150, 37.7800)),
  crs = 4326
)

route_with_barriers <- find_routes(
  stops,
  barriers = barrier_pts
)

# Accumulate multiple impedances
multi_impedance_route <- find_routes(
  stops,
  impedance_attribute_name = "travel_time",
  accumulate_attribute_names = c("miles", "kilometers")
)
} # }
```
