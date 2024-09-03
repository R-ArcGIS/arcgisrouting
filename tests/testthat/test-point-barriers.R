test_that("multiplication works", {
  xv <- runif(10, -10, 20)
  yv <- runif(10, -10, 20)

  x <- sf::st_as_sfc(wk::xy(xv, yv)) |> 
    sf::st_set_crs(4326) |> 
    sf::st_as_sf() 
  x[["travel_time"]] <- rnorm(10)
  x[["travel_time"]] <- rnorm(10)
  x[["name"]] <- letters[1:10]
  x[["dont_work"]] <- letters[1:10]

  expect_no_error(barriers <- as_point_barriers(x))
  expect_identical(
    yyjsonr::read_json_str(barriers)$attributes[[1]]$dont_work,
    NULL
  )

  # this shouldn't be allowed
  x[["full_edge"]] <- letters[1:10]
  expect_error(as_point_barriers(x))

  x[["miles"]] <- rep(c(TRUE, FALSE), 5)
  expect_error(as_point_barriers(x))
})
