load_credentials("credential.txt")

test_that("get fields works", {
  result <- get_fields('field1')$field_id %>% as.character()
  expect_equivalent(result, "field1")
})

test_that("get plantings works", {
  result <- get_planting(field_id = 'field1', planting_id = '73227')$planting_id
  expect_equivalent(result, 73227)
})
