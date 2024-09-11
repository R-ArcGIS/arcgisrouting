library(sf)
library(arcgisutils)
set_arc_token(auth_user())

# read sample dataset
points_spo <- read.csv(system.file("extdata/spo/spo_hexgrid.csv", package = "r5r"))

# create small sample dataset
origins <- st_as_sf(points_spo, coords = c("lon", "lat"), crs = 4326)["id"]
x <- od::od_data_coordinates |> 
  sf::st_as_sf(coords = c("X", "Y"), crs = 4326) |> 
  dplyr::rename(name = "geo_code") |> 
  dplyr::mutate("object_id" = 1:dplyr::n()) |> 
  head(3)

# job <- travel_cost_matrix_async(x, x, token = arc_token())

# job$start_job()

# job$job_status
# job$job_results

# download_od_results(job)



job <- service_areas_async(x, "Walking Time", token = auth_user())
job$start_job()
job$job_status
results <- download_service_area_results(job)

tmp <- results$service_areas

library(ggplot2)

travel_modes <- get_travel_modes(token = auth_user())

tm <- validate_travel_mode("Walking Time", token = arc_token()) 

jsonify::pretty_json(tm)
tmp |> 
  dplyr::mutate(breaks = paste0(from_break, " - ", to_break, " minutes (driving)")) |> 
  ggplot() +
  geom_sf(aes(fill = breaks), lwd = 0.1, color = NA, alpha = 0.7) +
  geom_sf(data = x, size = 0.5) + 
  scale_fill_viridis_d() + 
  theme_minimal() +
  theme(legend.position = "none")

