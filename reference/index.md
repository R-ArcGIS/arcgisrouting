# Package index

## Routing

Find the best route between an ordered or optimized set of stops and
generate turn-by-turn directions.

- [`find_routes()`](http://r.esri.com/arcgisrouting/reference/find_routes.md)
  : Find Routes
- [`find_routes_job()`](http://r.esri.com/arcgisrouting/reference/find_routes_job.md)
  : Find Routes (Async)

## Service areas

Generate polygons and lines describing the area reachable from a set of
facilities within a given travel time or distance.

- [`find_service_areas()`](http://r.esri.com/arcgisrouting/reference/find_service_areas.md)
  : Solve Service Area
- [`solve_service_areas_job()`](http://r.esri.com/arcgisrouting/reference/solve_service_areas_job.md)
  : Generate Service Areas (Async)
- [`download_service_area_results()`](http://r.esri.com/arcgisrouting/reference/download_service_area_results.md)
  : Download Service Area Results

## Closest facility

Find one or more nearby facilities from a set of incidents based on
travel time or distance.

- [`find_closest_facility()`](http://r.esri.com/arcgisrouting/reference/find_closest_facility.md)
  : Find Closest Facility
- [`find_closest_facilities_job()`](http://r.esri.com/arcgisrouting/reference/find_closest_facilities_job.md)
  : Find Closest Facilities (Async)

## Origin-destination cost matrix

Measure the travel time or distance between every origin and every
destination in a pair of point sets.

- [`travel_cost_matrix()`](http://r.esri.com/arcgisrouting/reference/travel_cost_matrix.md)
  : Travel Cost Matrix
- [`od_cost_matrix_job()`](http://r.esri.com/arcgisrouting/reference/od_cost_matrix_job.md)
  : Generate Origin-Destination Cost Matrix (Async)
- [`download_od_results()`](http://r.esri.com/arcgisrouting/reference/download_od_results.md)
  : Download Origin-Destination Cost Matrix Results

## Vehicle routing & last mile delivery

Plan efficient routes for a fleet of vehicles servicing a set of orders,
including clustered last mile delivery problems.

- [`route_vehicles()`](http://r.esri.com/arcgisrouting/reference/route_vehicles.md)
  : Route Vehicles
- [`solve_vrp_job()`](http://r.esri.com/arcgisrouting/reference/solve_vrp_job.md)
  : Solve Vehicle Routing Problem (Async)
- [`last_mile_delivery()`](http://r.esri.com/arcgisrouting/reference/last_mile_delivery.md)
  : Solve Last Mile Delivery (Async)

## Location-allocation

Choose the set of facilities that best serves demand from surrounding
areas.

- [`location_allocation_job()`](http://r.esri.com/arcgisrouting/reference/location_allocation_job.md)
  : Solve Location-Allocation (Async)

## Snap to roads

Snap a sequence of GPS points to the most likely roads traveled.

- [`snap_to_roads()`](http://r.esri.com/arcgisrouting/reference/snap_to_roads.md)
  : Snap Points to Roads

## Travel modes

Discover the modes of transportation available to your organization.

- [`get_travel_modes()`](http://r.esri.com/arcgisrouting/reference/get_travel_modes.md)
  : Get available travel modes
- [`retrieve_travel_modes()`](http://r.esri.com/arcgisrouting/reference/retrieve_travel_modes.md)
  : Retrieve Available Travel Modes

## Geometry helpers

Utilities for working with the geometries returned by the routing
services.

- [`decode_compressed_geometry()`](http://r.esri.com/arcgisrouting/reference/decode_compressed_geometry.md)
  : Decode Compressed Geometry
