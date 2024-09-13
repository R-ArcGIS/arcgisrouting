library(sf)
library(dplyr)
library(ggplot2)
library(arcgisutils)
library(arcgisrouting)
set_arc_token(auth_user())

# read sample dataset
points_spo <- readr::read_csv(
  system.file("extdata/spo/spo_hexgrid.csv", package = "r5r"),
  n_max = 3
)

# create small sample dataset
x <- st_as_sf(points_spo, coords = c("lon", "lat"), crs = 4326)

# Calculate travel matrix
travel_cost_matrix(x, x, token = arc_token())




job <- service_areas_async(x, "Walking Time", token = auth_user())

res <- poll_and_resolve_job(
  job$start(),
  on_completion = download_service_area_results
)


isochrones <- res$service_areas

isochrones |> 
  dplyr::mutate(breaks = paste0(from_break, " - ", to_break, " minutes (driving)")) |> 
  ggplot() +
  geom_sf(aes(fill = breaks), lwd = 0.1, color = NA, alpha = 0.7) +
  geom_sf(data = x, size = 1, color = "white") + 
  scale_fill_viridis_d() + 
  theme_void() +
  theme(legend.position = "none") +
  facet_wrap("facility_id")

