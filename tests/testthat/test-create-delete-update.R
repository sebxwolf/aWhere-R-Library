load_credentials("credential.txt")

test_that("create fields works", {
  create_field("field123","39.8282","-98.5795","farmA","Some Field Location","100")
  result <- get_fields('field123')$field_id %>% as.character()
  expect_equal(result, "field123")
})

test_that("create planting works", {
  id <- create_planting(field_id='field123',crop='corn',planting_date='2015-10-25',proj_yield_amount='100',proj_yield_units='Bushels', proj_harvest_date='2016-02-01',yield_amount='110',yield_units='Bushels',harvest_date='2016-02-01')
  result <- get_planting('field123', current = T)$planting_id
  expect_equal(result, id)
})

# test_that("create jobs works", {
#   id <- create_job(c("GET /v2/weather/fields/field123/observations", "GET /v2/weather/fields/field123/observations"), c("field123", "field123"), "job_1")
#   expect_type(get_job(id), "list")
# })

test_that("update field works", {
  update_field(field_id = 'field123',
               variable_search = 'farmId', value_search = 'farmA',
               variable_update = 'farmId', value_update = 'This is my territory')
  expect_equal("This is my territory", get_fields('field123')$farmId %>% as.character())
})

test_that("update planting works", {
  id <- get_planting('field123', current = T)$planting_id
  update_planting("field123", id, harvest_date = "2016-02-01", yield_amount = "60", yield_units = "Bushels")
  expect_equal(get_planting('field123', current = T)$actualHarvestDate %>% as.character(), "2016-02-01")
  expect_equal(get_planting('field123', current = T)$yieldAmount %>% as.character(), "60")
})

test_that("delete planting works", {
  result <- get_planting('field123', current = T)$planting_id
  delete_planting("field123", result)
  expect_error(get_planting('field123', result))
})

test_that("delete field works", {
  delete_field("field123")
  expect_error(get_fields('field123'))
})


