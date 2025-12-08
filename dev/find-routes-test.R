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
result <- find_routes(stops, return_stops = TRUE)

routes <- parse_esri_json(result, query = "/routes")
stops <- parse_esri_json(result, query = "/stops")
parse_esri_json(result, query = "/barriers")
parse_esri_json(result, query = "/traversedJunctions")
parse_esri_json(result, query = "/polylineBarriers")
parse_esri_json(result, query = "/polygonBarriers")
parse_esri_json(result, query = "/traversedEdges")
parse_esri_json(result, query = "/traversedTurns")
parse_esri_json(result, query = "/directionPoints")
parse_esri_json(result, query = "/directionLines")


routes <- parse_esri_json(result, query = "/directions")


parse_esri_json(result, query = "/stops")

res <- RcppSimdJson::fparse(result, query = "/directions") |> str(1)

RcppSimdJson::fparse(
  result,
  query = list(features = "/directions/features")
)

RcppSimdJson::fparse(result, query = "/directions/features")


clipr::write_clip(result)

tmp <- RcppSimdJson::fparse(result, query = "/directions/0/features") |>
  data_frame() |>
  dplyr::select(-compressedGeometry)

tmp$strings

arcgisutils::rbind_results(tmp$attributes)

tt <- list()
for (i in seq_along(tmp$attributes)) {
  tt[[i]] <- tibble::as_tibble(tmp$attributes[[i]])
}

directions <- rbind_results(tt) |>
  data_frame()

directions
