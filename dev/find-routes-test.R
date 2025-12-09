library(sf)
# Simple route between 3 stops

stops <- sf::st_sf(
  name = c("Start", "Middle", "End"),
  geometry = sf::st_sfc(
    sf::st_point(c(-122.4194, 37.7749)), # San Francisco
    sf::st_point(c(-122.0312, 37.3318)), # Cupertino
    sf::st_point(c(-121.8863, 37.3382)), # San Jose
    crs = 4326
  )
)
set_arc_token(auth_user())
result <- find_routes(
  stops,
  return_geometry = c(
    "routes",
    "directions",
    "stops",
    "barriers",
    "polyline_barriers",
    "polygon_barriers",
    "traversed_edges",
    "traversed_junctions",
    "traversed_turns"
  )
)


# Find optimal order for stops
sales_stops <- st_sf(
  name = c("Office", "Client 1", "Client 2", "Client 3", "Office"),
  geometry = st_sfc(
    st_point(c(-122.4194, 37.7749)),
    st_point(c(-122.4083, 37.7858)),
    st_point(c(-122.4313, 37.7793)),
    st_point(c(-122.4000, 37.7900)),
    st_point(c(-122.4194, 37.7749)),
    crs = 4326
  )
)

optimized_route <- find_routes(
  sales_stops,
  find_best_sequence = TRUE,
  preserve_first_stop = TRUE,
  preserve_last_stop = TRUE
)


all <- RcppSimdJson::fparse(optimized_route)

all$directions$features |> str()
res <- RcppSimdJson::fparse(result, query = "/directions") |>

  RcppSimdJson::fparse(result, query = "/directions/features")


# Route with time windows for deliveries
delivery_stops <- st_sf(
  name = c("Warehouse", "Customer A", "Customer B", "Customer C"),
  time_window_start = as.POSIXct(c(
    "2024-01-15 08:00:00",
    "2024-01-15 09:00:00",
    "2024-01-15 11:00:00",
    "2024-01-15 14:00:00"
  )),
  time_window_end = as.POSIXct(c(
    "2024-01-15 08:30:00",
    "2024-01-15 10:00:00",
    "2024-01-15 13:00:00",
    "2024-01-15 16:00:00"
  )),
  geometry = st_sfc(
    st_point(c(-122.4194, 37.7749)),
    st_point(c(-122.4083, 37.7858)),
    st_point(c(-122.4313, 37.7793)),
    st_point(c(-122.4000, 37.7900)),
    crs = 4326
  )
)

delivery_route <- find_routes(
  delivery_stops,
  start_time = as.POSIXct("2024-01-15 08:00:00"),
  travel_mode = "Driving Time"
)

clipr::write_clip(result)

tmp <- RcppSimdJson::fparse(result, query = "/directions/0/features") |>
  data_frame() |>
  dplyr::select(-compressedGeometry)


tmp$strings
tt <- list()
for (i in seq_along(tmp$attributes)) {
  tt[[i]] <- tibble::as_tibble(tmp$attributes[[i]])
}

directions <- rbind_results(tt) |>
  data_frame()

directions


# multiople routess ------------------------------------------------------
set_arc_token(auth_user())
multi_route_stops <- sf::st_sf(
  name = c("A1", "A2", "B1", "B2"),
  route_name = c("Route1", "Route1", "Route2", "Route2"),
  geometry = sf::st_sfc(
    sf::st_point(c(-122.4194, 37.7749)),
    sf::st_point(c(-122.0312, 37.3318)),
    sf::st_point(c(-121.8863, 37.3382)),
    sf::st_point(c(-121.9, 37.4)),
    crs = 4326
  )
)

result <- find_routes(multi_route_stops)

dirs_raw <- RcppSimdJson::fparse(result, query = "/directions")
str(dirs_raw, 2)

x <- dirs_raw$summary[[1]]

data_frame(vctrs::new_data_frame(x))
