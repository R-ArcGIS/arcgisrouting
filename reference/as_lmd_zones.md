# Convert spatial objects to Last Mile Delivery zones input

Zones specify a work territory as polygon features. The only recognized
attribute is `name`, which routes reference through their `zone_name`
attribute.

## Usage

``` r
as_lmd_zones(x, ...)
```

## Arguments

- x:

  An `sf` or `sfc` polygon object delineating work territories, or
  `NULL`.

- ...:

  Additional arguments passed to methods.
