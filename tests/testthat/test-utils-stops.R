test_that("as_stops() works with sfc_POINT", {
  pts <- sf::st_sfc(
    sf::st_point(c(-122.4, 37.8)),
    sf::st_point(c(-122.5, 37.9)),
    crs = 4326
  )

  expect_no_error(as_stops(pts))
})

test_that("as_stops() works with sf and name column", {
  stops_with_name <- sf::st_sf(
    name = c("Store A", "Store B", "Store C"),
    geometry = sf::st_sfc(
      sf::st_point(c(-122.4, 37.8)),
      sf::st_point(c(-122.5, 37.9)),
      sf::st_point(c(-122.6, 38.0)),
      crs = 4326
    )
  )

  expect_no_error(result <- as_stops(stops_with_name))
  parsed <- yyjsonr::read_json_str(result)
  expect_identical(
    unlist(parsed$features$attributes, use.names = FALSE),
    c("Store A", "Store B", "Store C")
  )
})

test_that("as_stops() works with multiple routes and sequence", {
  multi_route_stops <- sf::st_sf(
    name = c("A1", "A2", "B1", "B2", "B3"),
    route_name = c("Route1", "Route1", "Route2", "Route2", "Route2"),
    sequence = c(1, 2, 1, 2, 3),
    geometry = sf::st_sfc(
      sf::st_point(c(-122.4, 37.8)),
      sf::st_point(c(-122.5, 37.9)),
      sf::st_point(c(-122.6, 38.0)),
      sf::st_point(c(-122.7, 38.1)),
      sf::st_point(c(-122.8, 38.2)),
      crs = 4326
    )
  )

  expect_no_error(result <- as_stops(multi_route_stops))

  parsed <- yyjsonr::read_json_str(result)

  expect_identical(parsed$features$attributes[[1]]$RouteName, "Route1")
  expect_identical(parsed$features$attributes[[1]]$Sequence, 1L)
  expect_identical(parsed$features$attributes[[3]]$RouteName, "Route2")
})

test_that("as_stops() works with time windows", {
  stops_with_time <- sf::st_sf(
    name = c("Delivery 1", "Delivery 2"),
    time_window_start = as.POSIXct(c(
      "2024-01-15 09:00:00",
      "2024-01-15 14:00:00"
    )),
    time_window_end = as.POSIXct(c(
      "2024-01-15 12:00:00",
      "2024-01-15 17:00:00"
    )),
    geometry = sf::st_sfc(
      sf::st_point(c(-122.4, 37.8)),
      sf::st_point(c(-122.5, 37.9)),
      crs = 4326
    )
  )

  expect_no_error(result <- as_stops(stops_with_time))
})

test_that("as_stops() works with curb approach values", {
  stops_curb <- sf::st_sf(
    name = c("Right side", "Left side", "Either side", "No U-turn"),
    curb_approach = c(1L, 2L, 0L, 3L),
    geometry = sf::st_sfc(
      sf::st_point(c(-122.4, 37.8)),
      sf::st_point(c(-122.5, 37.9)),
      sf::st_point(c(-122.6, 38.0)),
      sf::st_point(c(-122.7, 38.1)),
      crs = 4326
    )
  )

  expect_no_error(result <- as_stops(stops_curb))

  parsed <- yyjsonr::read_json_str(result)
  expect_identical(parsed$features$attributes[[1]]$CurbApproach, 1L)
  expect_identical(parsed$features$attributes[[2]]$CurbApproach, 2L)
  expect_identical(parsed$features$attributes[[3]]$CurbApproach, 0L)
  expect_identical(parsed$features$attributes[[4]]$CurbApproach, 3L)
})

test_that("as_stops() works with location types", {
  stops_types <- sf::st_sf(
    name = c("Stop", "Waypoint", "Break"),
    location_type = c(0L, 1L, 2L),
    geometry = sf::st_sfc(
      sf::st_point(c(-122.4, 37.8)),
      sf::st_point(c(-122.5, 37.9)),
      sf::st_point(c(-122.6, 38.0)),
      crs = 4326
    )
  )

  expect_no_error(result <- as_stops(stops_types))

  parsed <- yyjsonr::read_json_str(result)
  expect_identical(parsed$features$attributes[[1]]$LocationType, 0L)
  expect_identical(parsed$features$attributes[[2]]$LocationType, 1L)
  expect_identical(parsed$features$attributes[[3]]$LocationType, 2L)
})

test_that("as_stops() works with bearing and bearing tolerance", {
  stops_bearing <- sf::st_sf(
    name = c("Moving vehicle 1", "Moving vehicle 2"),
    bearing = c(45, 90),
    bearing_tol = c(15, 30),
    geometry = sf::st_sfc(
      sf::st_point(c(-122.4, 37.8)),
      sf::st_point(c(-122.5, 37.9)),
      crs = 4326
    )
  )

  expect_no_error(result <- as_stops(stops_bearing))

  parsed <- yyjsonr::read_json_str(result)
  expect_identical(parsed$features$attributes[[1]]$Bearing, 45)
  expect_identical(parsed$features$attributes[[1]]$BearingTol, 15)
})

test_that("as_stops() works with bearing, bearing_tol, and nav_latency", {
  stops_nav <- sf::st_sf(
    name = c("GPS point 1", "GPS point 2"),
    bearing = c(45, 90),
    bearing_tol = c(15, 30),
    nav_latency = c(5, 10),
    geometry = sf::st_sfc(
      sf::st_point(c(-122.4, 37.8)),
      sf::st_point(c(-122.5, 37.9)),
      crs = 4326
    )
  )

  expect_no_error(result <- as_stops(stops_nav))

  parsed <- yyjsonr::read_json_str(result)
  expect_identical(parsed$features$attributes[[1]]$NavLatency, 5)
})

test_that("as_stops() works with ObjectID variations", {
  stops_oid <- sf::st_sf(
    object_id = 1:3,
    geometry = sf::st_sfc(
      sf::st_point(c(-122.4, 37.8)),
      sf::st_point(c(-122.5, 37.9)),
      sf::st_point(c(-122.6, 38.0)),
      crs = 4326
    )
  )

  expect_no_error(result <- as_stops(stops_oid))

  parsed <- yyjsonr::read_json_str(result)
  expect_identical(parsed$features$attributes[[1]]$ObjectID, 1L)
})

test_that("as_stops() works with comprehensive attributes", {
  stops_comprehensive <- sf::st_sf(
    name = c("Delivery A", "Delivery B", "Delivery C"),
    route_name = c("Route1", "Route1", "Route2"),
    sequence = c(1, 2, 1),
    time_window_start = as.POSIXct(c(
      "2024-01-15 09:00:00",
      "2024-01-15 11:00:00",
      "2024-01-15 14:00:00"
    )),
    time_window_end = as.POSIXct(c(
      "2024-01-15 10:00:00",
      "2024-01-15 12:00:00",
      "2024-01-15 16:00:00"
    )),
    curb_approach = c(1L, 2L, 0L),
    location_type = c(0L, 0L, 0L),
    geometry = sf::st_sfc(
      sf::st_point(c(-122.4, 37.8)),
      sf::st_point(c(-122.5, 37.9)),
      sf::st_point(c(-122.6, 38.0)),
      crs = 4326
    )
  )

  expect_no_error(result <- as_stops(stops_comprehensive))

  parsed <- yyjsonr::read_json_str(result)
  expect_identical(parsed$features$attributes[[1]]$Name, "Delivery A")
  expect_identical(parsed$features$attributes[[1]]$RouteName, "Route1")
  expect_identical(parsed$features$attributes[[1]]$Sequence, 1L)
})

test_that("as_stops() ignores unrecognized columns", {
  stops_extra <- sf::st_sf(
    name = c("Stop A", "Stop B"),
    unrecognized_col = c("foo", "bar"),
    geometry = sf::st_sfc(
      sf::st_point(c(-122.4, 37.8)),
      sf::st_point(c(-122.5, 37.9)),
      crs = 4326
    )
  )

  expect_no_error(result <- as_stops(stops_extra))

  parsed <- yyjsonr::read_json_str(result)
  expect_identical(parsed$features$attributes[[1]]$unrecognized_col, NULL)
})

test_that("as_stops() errors on non-point geometries", {
  line <- sf::st_sfc(
    sf::st_linestring(matrix(
      c(-122.4, 37.8, -122.5, 37.9),
      ncol = 2,
      byrow = TRUE
    )),
    crs = 4326
  )

  expect_error(as_stops(line), "point geometries")
})

test_that("as_stops() errors on missing CRS", {
  pts_no_crs <- sf::st_sfc(sf::st_point(c(-122.4, 37.8)))

  expect_error(as_stops(pts_no_crs), "crs")
})

test_that("as_stops() errors on invalid curb_approach value", {
  stops_bad_curb <- sf::st_sf(
    curb_approach = c(5L),
    geometry = sf::st_sfc(sf::st_point(c(-122.4, 37.8)), crs = 4326)
  )

  expect_error(as_stops(stops_bad_curb))
})

test_that("as_stops() errors on invalid location_type", {
  stops_bad_type <- sf::st_sf(
    location_type = c(5L),
    geometry = sf::st_sfc(sf::st_point(c(-122.4, 37.8)), crs = 4326)
  )

  expect_error(as_stops(stops_bad_type))
})

test_that("as_stops() errors on bearing_tol out of range", {
  stops_bad_bearing_tol <- sf::st_sf(
    bearing_tol = c(200),
    geometry = sf::st_sfc(sf::st_point(c(-122.4, 37.8)), crs = 4326)
  )

  expect_error(as_stops(stops_bad_bearing_tol), "between 0 and 180")
})

test_that("as_stops() errors on nav_latency without bearing and bearing_tol", {
  stops_bad_nav <- sf::st_sf(
    nav_latency = c(5),
    geometry = sf::st_sfc(sf::st_point(c(-122.4, 37.8)), crs = 4326)
  )

  expect_error(as_stops(stops_bad_nav), "bearing")
})

test_that("as_stops() errors on invalid character type for name", {
  stops_bad_name <- sf::st_sf(
    name = c(1, 2, 3),
    geometry = sf::st_sfc(
      sf::st_point(c(-122.4, 37.8)),
      sf::st_point(c(-122.5, 37.9)),
      sf::st_point(c(-122.6, 38.0)),
      crs = 4326
    )
  )

  expect_error(as_stops(stops_bad_name))
})
