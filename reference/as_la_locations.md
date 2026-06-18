# Convert spatial objects to facilities or demand points

Prepares `sf` or `sfc` point geometries as facilities or demand points
for
[`location_allocation_job()`](http://r.esri.com/arcgisrouting/reference/location_allocation_job.md),
recognizing snake_case attribute columns and validating their types
before serializing to an Esri feature set.

## Usage

``` r
as_la_facilities(x, ...)

as_demand_points(x, ...)
```

## Arguments

- x:

  An `sf` or `sfc` object containing point geometries.

- ...:

  Additional arguments passed to methods.

- verbose:

  Logical. Whether to message which recognized attributes are used.
  Default: `TRUE`.
