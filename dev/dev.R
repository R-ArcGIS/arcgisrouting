library(sf)
library(dplyr)
library(ggplot2)
library(arcgisutils)
library(arcgisrouting)


set_arc_token(auth_user())

# read sample dataset
x <- readr::read_csv(
  system.file("extdata/spo/spo_hexgrid.csv", package = "r5r"),
  n_max = 3
) |> st_as_sf(coords = c("lon", "lat"), crs = 4326)


# Calculate travel matrix
travel_cost_matrix(origins = x)


# Calculate a service area 
job <- service_areas_async(x, "Walking Time", token = auth_user())

res <- poll_and_resolve_job(
  job$start(),
  on_completion = download_service_area_results
)

# extract the service areas
isochrones <- res$service_areas

isochrones |> 
  ggplot() +
  geom_sf(aes(fill = as.factor(from_break)), lwd = 0.1, color = NA, alpha = 0.7) +
  scale_fill_viridis_d() + 
  theme_void() +
  theme(legend.position = "none") +
  facet_wrap("facility_id")

