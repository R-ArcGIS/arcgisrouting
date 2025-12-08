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
result <- find_routes(stops)


decode_compressed_geometry(result$directions$features[[1]]$compressedGeometry[
  5
])
