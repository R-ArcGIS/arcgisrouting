# Find Closest Facilities (Async)

Submits an asynchronous geoprocessing job that finds one or more nearby
facilities from incidents based on travel time or travel distance using
the ArcGIS `/FindClosestFacilities` GP service.

## Usage

``` r
find_closest_facilities_job(
  incidents,
  facilities,
  travel_mode = NULL,
  number_of_facilities_to_find = NULL,
  cutoff = NULL,
  travel_direction = NULL,
  measurement_units = NULL,
  analysis_region = NULL,
  time_of_day = NULL,
  time_of_day_usage = NULL,
  uturn_at_junctions = NULL,
  use_hierarchy = NULL,
  restrictions = NULL,
  attribute_parameter_values = NULL,
  time_impedance = NULL,
  distance_impedance = NULL,
  impedance = NULL,
  point_barriers = NULL,
  line_barriers = NULL,
  polygon_barriers = NULL,
  route_shape = NULL,
  route_line_simplification_tolerance = NULL,
  populate_directions = NULL,
  directions_language = NULL,
  directions_distance_units = NULL,
  directions_style_name = NULL,
  save_route_data = NULL,
  save_output_network_analysis_layer = NULL,
  output_format = "feature_set",
  ignore_invalid_locations = TRUE,
  token = arcgisutils::arc_token()
)
```

## Arguments

- incidents:

  An `sf` or `sfc` object containing point geometries representing the
  locations from which the nearby facilities are searched.

- facilities:

  An `sf` or `sfc` object containing point geometries representing the
  locations that are searched for when finding the closest location.

- travel_mode:

  Character. The name or ID of the travel mode to use. See
  [`get_travel_modes()`](http://r.esri.com/arcgisrouting/reference/get_travel_modes.md)
  for available options. Default: `NULL`.

- number_of_facilities_to_find:

  Integer. The number of closest facilities to find per incident.
  Default: `NULL` (API default: `1`).

- cutoff:

  Numeric. The travel time or travel distance value at which to stop
  searching for facilities for a given incident. Units are determined by
  `measurement_units`. Default: `NULL` (API default: no cutoff).

- travel_direction:

  Character. Direction the closest facility search is measured. One of:
  `"facility"` (from facilities to incidents) or `"incident"` (from
  incidents to facilities). Default: `NULL` (API default: `"incident"`).

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

- time_of_day:

  POSIXct. The departure time for the routes. When `NULL`, static
  average speeds are used. Default: `NULL`.

- time_of_day_usage:

  Character. Whether `time_of_day` represents the departure or arrival
  time of the routes. One of: `"start"` (departure) or `"end"`
  (arrival). Default: `NULL` (API default: `"start"`).

- uturn_at_junctions:

  Character. U-turn policy at junctions. One of: `"allow_backtrack"`,
  `"deadend_intersection"`, `"deadend"`, `"no_backtrack"`. Default:
  `NULL`.

- use_hierarchy:

  Logical. Whether to use the street hierarchy when finding routes.
  Default: `NULL` (API default: `TRUE`).

- restrictions:

  Character vector. Restriction names to apply. Default: `NULL`.

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

- route_shape:

  Character. Shape of the output route features. One of: `"true_shape"`,
  `"true_shape_with_measures"`, `"straight_line"`, `"none"`. Default:
  `NULL` (API default: `"true_shape"`).

- route_line_simplification_tolerance:

  List with elements `distance` (numeric) and `units` (character).
  Simplification tolerance for the output route geometry. Default:
  `NULL` (API default: `10` meters).

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

- save_route_data:

  Logical. Whether the route data is saved as a `.zip` file. Default:
  `NULL` (API default: `FALSE`).

- output_format:

  Character. Format for output features. One of: `"feature_set"`,
  `"json_file"`, `"geojson_file"`. Default: `"feature_set"`.

- ignore_invalid_locations:

  Logical. Whether to ignore invalid input locations. Default: `TRUE`.

- token:

  Authorization token. Default:
  [`arcgisutils::arc_token()`](https://rdrr.io/pkg/arcgisutils/man/token.html).

## Value

A `find_closest_facilities_job` R6 object inheriting from
[`arcgisutils::arc_gp_job`](https://rdrr.io/pkg/arcgisutils/man/gp_job.html).
Call `$start()` to submit and `$results` to retrieve output.

## References

[API
Reference](https://developers.arcgis.com/rest/routing/closestFacility-service-job/)

## See also

Other async:
[`download_od_results()`](http://r.esri.com/arcgisrouting/reference/download_od_results.md),
[`download_service_area_results()`](http://r.esri.com/arcgisrouting/reference/download_service_area_results.md),
[`find_routes_job()`](http://r.esri.com/arcgisrouting/reference/find_routes_job.md),
[`find_service_areas_job()`](http://r.esri.com/arcgisrouting/reference/find_service_areas_job.md),
[`last_mile_delivery()`](http://r.esri.com/arcgisrouting/reference/last_mile_delivery.md),
[`location_allocation_job()`](http://r.esri.com/arcgisrouting/reference/location_allocation_job.md),
[`od_cost_matrix_job()`](http://r.esri.com/arcgisrouting/reference/od_cost_matrix_job.md),
[`route_vehicles_job()`](http://r.esri.com/arcgisrouting/reference/route_vehicles_job.md)

Other closest facility:
[`find_closest_facilities()`](http://r.esri.com/arcgisrouting/reference/find_closest_facilities.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(sf)
library(arcgisutils)
set_arc_token(auth_user())

incidents <- st_sfc(st_point(c(-122.4496, 37.7467)), crs = 4326)

facilities <- st_sf(
  name = c("Station 11", "Station 20", "Station 24", "Station 39"),
  geometry = st_sfc(
    st_point(c(-122.4267, 37.7486)),
    st_point(c(-122.4561, 37.7513)),
    st_point(c(-122.4409, 37.7533)),
    st_point(c(-122.4578, 37.7407)),
    crs = 4326
  )
)

job <- find_closest_facilities_job(
  incidents,
  facilities,
  number_of_facilities_to_find = 2,
  travel_direction = "facility",
  cutoff = 5,
  measurement_units = "minutes",
  populate_directions = TRUE
)
job$start()
result <- job$results
} # }
```
