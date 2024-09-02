test_that("time of day checks", {
  expect_no_error(check_time_of_day(NULL))
  expect_no_error(check_time_of_day(Sys.time()))

  # these are invalid
  expect_error(check_time_of_day(rep(Sys.time(), 2)))
  expect_error(check_time_of_day("3245"))
  expect_error(check_time_of_day(list()))
})

test_that("validate u turn arguments", {
  expect_identical(validate_u_turns("allow_backtrack"), "esriNFSBAllowBacktrack")
  expect_identical(validate_u_turns("deadend_intersection"), "esriNFSBAtDeadEndsAndIntersections")
  expect_identical(validate_u_turns("deadend"), "esriNFSBAtDeadEndsOnly")
  expect_identical(validate_u_turns("no_backtrack"), "esriNFSBNoBacktrack")
  expect_identical(validate_u_turns(NULL), NULL)
  expect_error(validate_u_turns(1))
  expect_error(validate_u_turns(c("deadend", "allow_backtrack")))
})
