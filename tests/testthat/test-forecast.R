load_credentials("credential.txt")

test_that("get agonomic daily works", {
  create_field("field123","39.8282","-98.5795","farmA","Some Field Location","100")

  result <- forecasts_fields('field123')
  expect_is(result, "data.frame")

  result <- forecasts_fields('field123', block_size = 12)
  expect_is(result, "data.frame")

  result <- forecasts_fields('field123', as.character(Sys.Date()+3), block_size = 12)
  expect_is(result, "data.frame")
})


test_that("get weather norm works", {
  result <- forecasts_latlng('39.8282', '-98.5795')
  expect_is(result, "data.frame")

  result <- forecasts_latlng('39.8282', '-98.5795', block_size = 12)
  expect_is(result, "data.frame")

  result <- forecasts_latlng('39.8282', '-98.5795', as.character(Sys.Date()+3), block_size = 12)
  expect_is(result, "data.frame")

  # clean up
  delete_field("field123")
})
