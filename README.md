

<!-- README.md is generated from README.qmd. Edit that file, then run `quarto render README.qmd`. -->

# arcgisrouting <a href="https://r.esri.com/arcgisrouting/"><img src="man/figures/logo.png" align="right" height="120" alt="arcgisrouting website" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/r-arcgis/arcgisrouting/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/r-arcgis/arcgisrouting/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

`arcgisrouting` brings the [ArcGIS Routing REST
API](https://developers.arcgis.com/rest/routing/) to R. Plan routes,
measure travel time and distance, build service areas, route fleets of
vehicles, and snap GPS tracks to roads, all returning
[`sf`](https://r-spatial.github.io/sf/) objects ready for analysis and
mapping.

It is part of the [R-ArcGIS
Bridge](https://developers.arcgis.com/r-bridge/).

> [!IMPORTANT]
>
> Using `arcgisrouting` requires an ArcGIS Online account, an ArcGIS
> Location Platform account, or an ArcGIS Enterprise server. Routing
> operations consume ArcGIS credits. For pricing, see the [ArcGIS
> Routing pricing
> details](https://developers.arcgis.com/rest/routing/#pricing).

## Installation

Install the development version from GitHub:

``` r
# install.packages("pak")
pak::pak("r-arcgis/arcgisrouting")
```

## Authentication

Every request is authorized with an ArcGIS token. Authenticate once per
session with [`arcgisutils`](https://github.com/r-arcgis/arcgisutils)
and the package will pick the token up automatically.

``` r
library(arcgisutils)
library(arcgisrouting)

set_arc_token(auth_user())
```

See the [R-ArcGIS Bridge authentication
guide](https://developers.arcgis.com/r-bridge/authentication/overview/)
for client credentials, API keys, and other workflows.

## Quick start

Find the best route between an ordered set of stops:

``` r
library(sf)
library(arcgisutils)
library(arcgisrouting)

set_arc_token(auth_user())

stops <- st_sf(
  name = c("Ferry Building", "Coit Tower", "Painted Ladies"),
  geometry = st_sfc(
    st_point(c(-122.3933, 37.7955)),
    st_point(c(-122.4058, 37.8024)),
    st_point(c(-122.4329, 37.7762)),
    crs = 4326
  )
)

route <- find_routes(stops)
route$routes
```

## What you can do

| Capability | Direct Requests | Asynchronous Geoprocessing Job |
|----|----|----|
| Routing and directions | `find_routes()` | `find_routes_job()` |
| Service areas | `find_service_areas()` | `find_service_areas_job()` |
| Closest facility | `find_closest_facilities()` | `find_closest_facilities_job()` |
| Origin-destination cost matrix | `od_cost_matrix()` | `od_cost_matrix_job()` |
| Vehicle routing problem | `route_vehicles()` | `route_vehicles_job()` |
| Last mile delivery |  | `last_mile_delivery()` |
| Location-allocation |  | `location_allocation_job()` |
| Snap GPS points to roads | `snap_to_roads()` |  |

The **direct request** functions return immediately and are best for
interactive work and modest inputs. The **asynchronous geoprocessing
job** functions queue a job on the server, which is ideal for large
problems.

Working with a job looks like this:

``` r
job <- find_routes_job(stops)

job$start()      # submit the job to the server
job$await()      # poll until the job completes
job$messages()   # retrieve the geoprocessing job messages
job$results      # read the results once the job has finished
```

Discover the travel modes available to your organization with
`get_travel_modes()`.

## Endpoint coverage

### Direct services

| Endpoint                     | Status |
|------------------------------|--------|
| `/solve`                     | ✅     |
| `/solveServiceArea`          | ✅     |
| `/EditVehicleRoutingProblem` | ✅     |
| `/solveClosestFacility`      | ✅     |
| `/solveODCostMatrix`         | ✅     |
| `/GetTravelModes`            | ✅     |
| `/retrieveTravelModes`       | ✅     |
| `/SnapToRoads`               | ✅     |
| `/GetToolInfo`               |        |
| `/Traffic`                   |        |

### Asynchronous services (geoprocessing jobs)

| Endpoint                               | Status |
|----------------------------------------|--------|
| `/FindRoutes`                          | ✅     |
| `/GenerateServiceAreas`                | ✅     |
| `/SolveVehicleRoutingProblem`          | ✅     |
| `/SolveLastMileDelivery`               | ✅     |
| `/FindClosestFacilities`               | ✅     |
| `/SolveLocationAllocation`             | ✅     |
| `/GenerateOriginDestinationCostMatrix` | ✅     |

## ArcGIS Online and Enterprise

This package is built and tested against ArcGIS Online. It may not yet
work with ArcGIS Enterprise. If you run into trouble using
`arcgisrouting` with Enterprise, please [open an
issue](https://github.com/r-arcgis/arcgisrouting/issues) or, if you
would rather reach us privately, email <r_bridge@esri.com>.

## Learn more

- [Package website](https://r.esri.com/arcgisrouting/)
- [ArcGIS Routing REST API
  reference](https://developers.arcgis.com/rest/routing/)
- [R-ArcGIS Bridge](https://developers.arcgis.com/r-bridge/)
