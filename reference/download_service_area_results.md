# Download Service Area Results

Downloads and unzips the result file produced by a completed service
area geoprocessing job, parsing each JSON output into a named list of
data frames with snake_case column names.

## Usage

``` r
download_service_area_results(job)
```

## Arguments

- job:

  A completed service area job object.

## Value

A named list of data frames, one per JSON output, with snake_case names
derived from the output file names.

## See also

Other async:
[`download_od_results()`](http://r.esri.com/arcgisrouting/reference/download_od_results.md),
[`find_closest_facilities_job()`](http://r.esri.com/arcgisrouting/reference/find_closest_facilities_job.md),
[`find_routes_job()`](http://r.esri.com/arcgisrouting/reference/find_routes_job.md),
[`last_mile_delivery()`](http://r.esri.com/arcgisrouting/reference/last_mile_delivery.md),
[`location_allocation_job()`](http://r.esri.com/arcgisrouting/reference/location_allocation_job.md),
[`od_cost_matrix_job()`](http://r.esri.com/arcgisrouting/reference/od_cost_matrix_job.md),
[`solve_service_areas_job()`](http://r.esri.com/arcgisrouting/reference/solve_service_areas_job.md),
[`solve_vrp_job()`](http://r.esri.com/arcgisrouting/reference/solve_vrp_job.md)

Other service area:
[`find_service_areas()`](http://r.esri.com/arcgisrouting/reference/find_service_areas.md),
[`solve_service_areas_job()`](http://r.esri.com/arcgisrouting/reference/solve_service_areas_job.md)
