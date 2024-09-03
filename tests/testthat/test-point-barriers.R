test_that("multiplication works", {
  xv <- runif(10, -10, 20)
  yv <- runif(10, -10, 20)

  x <- sf::st_as_sfc(wk::xy(xv, yv)) |> 
    sf::st_set_crs(4326) |> 
    sf::st_as_sf() 

  # single column sf should dispatch to sfc
  expect_no_errors(as_point_barriers(sf::st_as_sf(x)))

  # add impedance 
  x[["travel_time"]] <- rnorm(10)
  x[["travel_time"]] <- rnorm(10)
  # name
  x[["name"]] <- letters[1:10]
  # fields that shouldnt work
  x[["dont_work"]] <- letters[1:10]

  # check that no error occurs
  expect_no_error(barriers <- as_point_barriers(x))
  # we expect that dont_work wasn't created
  expect_identical(
    yyjsonr::read_json_str(barriers)$attributes[[1]]$dont_work,
    NULL
  )

  # this shouldn't be allowed
  x[["full_edge"]] <- letters[1:10]
  expect_error(as_point_barriers(x))
  
  # only permit numerics 
  x[["miles"]] <- rep(c(TRUE, FALSE), 5)
  expect_error(as_point_barriers(x))

  
})

