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
  head(10)

job <- travel_cost_matrix_async(x, x, token = arc_token())

job$start_job()

job$job_status
job$job_results

download_od_results(job)

