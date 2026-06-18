# Retrieve Available Travel Modes

Retrieves the travel modes supported by the routing services associated
with the provided token.

## Usage

``` r
retrieve_travel_modes(
  token = arcgisutils::arc_token(),
  error_call = rlang::caller_call()
)
```

## Arguments

- token:

  Authorization token. Default:
  [`arcgisutils::arc_token()`](https://rdrr.io/pkg/arcgisutils/man/token.html).

## Value

A list describing the supported travel modes, including a
`supportedTravelModes` data frame.

## See also

Other travel modes:
[`get_travel_modes()`](http://r.esri.com/arcgisrouting/reference/get_travel_modes.md)
