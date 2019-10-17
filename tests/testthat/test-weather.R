load_credentials("credential.txt")

test_that("get weather daily works", {
  create_field("field123","39.8282","-98.5795","farmA","Some Field Location","100")

  result <- daily_observed_fields('field123')
  expect_is(result, "data.frame")

  result <- daily_observed_fields('field123', '2015-03-01')
  expect_is(result, "data.frame")

  result <- daily_observed_fields('field123','2015-03-01','2015-05-01')
  expect_is(result, "data.frame")
})

test_that("get weather daily works", {
  result <- daily_observed_latlng('39.8282', '-98.5795')
  expect_is(result, "data.frame")

  result <- daily_observed_latlng('39.8282', '-98.5795', '2015-03-01')
  expect_is(result, "data.frame")

  result <- daily_observed_latlng('39.8282', '-98.5795','2015-03-01','2015-05-01')
  expect_is(result, "data.frame")
})

test_that("get weather norm works", {
  result <- weather_norms_fields('field123', monthday_start = '07-01', monthday_end = '07-10',  year_start = '2008', year_end = '2015')
  expect_is(result, "data.frame")

  result <- weather_norms_fields('field123', monthday_start = '07-01', monthday_end = '07-10', year_start = '2008', year_end = '2015', exclude_years = '2009,2013')
  expect_is(result, "data.frame")
})

test_that("get weather norm works", {
  result <- weather_norms_latlng('39.8282', '-98.5795', monthday_start = '07-01', monthday_end = '07-10',  year_start = '2008', year_end = '2015')
  expect_is(result, "data.frame")

  result <- weather_norms_latlng('39.8282', '-98.5795', monthday_start = '07-01', monthday_end = '07-10', year_start = '2008', year_end = '2015', exclude_years = '2009,2013')
  expect_is(result, "data.frame")

  # clean up
  delete_field("field123")
})
