# Download Origin-Destination Cost Matrix Results

Downloads and unzips the result file produced by a completed
origin-destination cost matrix geoprocessing job, reading each CSV
output into a named list of data frames with snake_case column names.

## Usage

``` r
download_od_results(job)
```

## Arguments

- job:

  A completed origin-destination cost matrix job object.

## Value

A named list of data frames, one per CSV output, with snake_case names
derived from the output file names.

## See also

Other async:
[`download_service_area_results()`](http://r.esri.com/arcgisrouting/reference/download_service_area_results.md),
[`find_closest_facilities_job()`](http://r.esri.com/arcgisrouting/reference/find_closest_facilities_job.md),
[`find_routes_job()`](http://r.esri.com/arcgisrouting/reference/find_routes_job.md),
[`find_service_areas_job()`](http://r.esri.com/arcgisrouting/reference/find_service_areas_job.md),
[`last_mile_delivery()`](http://r.esri.com/arcgisrouting/reference/last_mile_delivery.md),
[`location_allocation_job()`](http://r.esri.com/arcgisrouting/reference/location_allocation_job.md),
[`od_cost_matrix_job()`](http://r.esri.com/arcgisrouting/reference/od_cost_matrix_job.md),
[`route_vehicles_job()`](http://r.esri.com/arcgisrouting/reference/route_vehicles_job.md)

Other od:
[`od_cost_matrix()`](http://r.esri.com/arcgisrouting/reference/od_cost_matrix.md),
[`od_cost_matrix_job()`](http://r.esri.com/arcgisrouting/reference/od_cost_matrix_job.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# This example is not executed since it requires a network connection
# to ArcGIS Online and a valid authentication token
library(sf)
library(arcgisutils)
set_arc_token(auth_user())

origins <- st_sfc(
  st_point(c(-122.4194, 37.7749)),
  st_point(c(-122.4313, 37.7793)),
  crs = 4326
)

destinations <- st_sfc(
  st_point(c(-122.4083, 37.7858)),
  st_point(c(-122.4000, 37.7900)),
  crs = 4326
)

job <- od_cost_matrix_job(origins, destinations)
job$start()
job$await()

download_od_results(job)
} # }
```
