# Get available travel modes

Returns the names of the travel modes supported by the routing services
associated with the provided token.

## Usage

``` r
get_travel_modes(token = arc_token())
```

## Arguments

- token:

  Authorization token. Default:
  [`arcgisutils::arc_token()`](https://rdrr.io/pkg/arcgisutils/man/token.html).

## Value

A character vector of supported travel mode names.

## See also

Other travel modes:
[`retrieve_travel_modes()`](http://r.esri.com/arcgisrouting/reference/retrieve_travel_modes.md)
