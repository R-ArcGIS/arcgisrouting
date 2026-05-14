library(sf)
library(arcgisutils)
devtools::load_all()

set_arc_token(auth_user())

pts <- st_sf(
  location_timestamp = as.POSIXct(
    c(1704522159, 1704522171, 1704522183, 1704522195, 1704522231, 1704522247),
    origin = "1970-01-01"
  ),
  geometry = st_sfc(
    st_point(c(-122.43410099956145, 37.800155000053394)),
    st_point(c(-122.43459799999755, 37.800737999958812)),
    st_point(c(-122.43539199974094, 37.800955000073259)),
    st_point(c(-122.43629900019869, 37.800787999566637)),
    st_point(c(-122.43627299989902, 37.800661000005221)),
    st_point(c(-122.43612999959976, 37.800168999799666)),
    crs = 4326
  ),
  track_id = sort(rep(ulid::ulid(2), 3))
)

result <- snap_to_roads(pts)

result$snap_lines$geometry |> plot()
result$snapped_points$geometry |> plot(add = TRUE)
