test_that("time of day checks", {
  check_time_of_day(NULL)
  check_time_of_day(Sys.time())
  check_time_of_day(rep(Sys.time(), 2))
  check_time_of_day("3245")
  check_time_of_day(list())
})
