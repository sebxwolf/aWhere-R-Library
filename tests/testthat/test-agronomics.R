load_credentials("credential.txt")

test_that("get agonomic daily works", {
  create_field("field123","39.8282","-98.5795","farmA","Some Field Location","100")

  result <- agronomic_values_fields('field123','2015-07-01','2015-07-31')
  expect_is(result, "data.frame")

  result <- agronomic_values_fields('field123', '2015-03-01')
  expect_is(result, "data.frame")

  result <- agronomic_values_fields('field123','2015-03-01','2015-05-01','','standard','10','10','30')
  expect_is(result, "data.frame")

  result <- agronomic_values_fields("field123", day_start = "2016-07-01", day_end = "2016-07-31", accumulation_start_date = "2016-06-01", gdd_method = "modifiedstandard", gdd_base_temp = "10", gdd_min_boundary = "10", gdd_max_boundary = "30")
  expect_is(result, "data.frame")
})

test_that("get agonomic daily works", {
  result <- agronomic_values_latlng('39.8282', '-98.5795','2015-07-01','2015-07-31')
  expect_is(result, "data.frame")

  result <- agronomic_values_latlng('39.8282', '-98.5795', '2015-03-01')
  expect_is(result, "data.frame")

  result <- agronomic_values_latlng('39.8282', '-98.5795','2015-03-01','2015-05-01','','standard','10','10','30')
  expect_is(result, "data.frame")

  result <- agronomic_values_latlng('39.8282', '-98.5795', day_start = "2016-07-01", day_end = "2016-07-31", accumulation_start_date = "2016-06-01", gdd_method = "modifiedstandard", gdd_base_temp = "10", gdd_min_boundary = "10", gdd_max_boundary = "30")
  expect_is(result, "data.frame")
})

test_that("get agronomic_norms_fields norm works", {
  result <- agronomic_norms_fields('field123','07-01', '07-10', '2008', '2015','2010,2011','','standard','10','10','30')

  expect_is(result, "data.frame")
})

test_that("get agronomic_norms_latlng norm works", {
  result <- agronomic_norms_latlng('39.8282', '-98.5795','07-01', '07-10', '2008', '2015','2010,2011','','standard','10','10','30')

  expect_is(result, "data.frame")
  # clean up
  delete_field("field123")
})
