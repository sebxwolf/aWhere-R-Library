## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ------------------------------------------------------------------------

suppressPackageStartupMessages(library(aWhereAPI))


## ---- eval=FALSE---------------------------------------------------------
#  load_credentials("C:/Users/Hannacamp/Documents/R Working Directory/API/credentials.txt")

## ---- echo=FALSE---------------------------------------------------------
get_token("yizhexu@awhere.com", "181225tiancai@X")

## ------------------------------------------------------------------------

 create_field("field_test","38.8282","-102.5795","farmA")


## ------------------------------------------------------------------------

get_fields("field_test")


## ------------------------------------------------------------------------

# update_field("field_test", "farmId", "farmA", "farmId", "TestFarmA")

get_fields("field_test")


## ------------------------------------------------------------------------
    create_planting("field_test", "corn", "2015-07-01")


## ------------------------------------------------------------------------
get_planting("field_test")


## ------------------------------------------------------------------------
daily_observed_fields("field_test", "2015-07-01")

daily_observed_fields("field_test", day_start = "2015-07-01", day_end = "2015-07-07")

## ------------------------------------------------------------------------
daily_observed_latlng(latitude = "39.8282", longitude = "-98.5795", day_start = "2015-07-01", day_end = "2015-07-07")


## ------------------------------------------------------------------------
forecasts_fields("field_test", as.character(Sys.Date() + 1), block_size = 4)

forecasts_fields("field_test", day_start = as.character(Sys.Date() + 1), day_end = as.character(Sys.Date() + 5), block_size = 12)


## ------------------------------------------------------------------------
forecasts_latlng(latitude = "39.8282", longitude = "-98.5795", day_start = as.character(Sys.Date() + 1), block_size = 24)


## ------------------------------------------------------------------------
weather_norms_fields(field_id = "field_test", monthday_start = "07-01", monthday_end = "07-31", year_start = "2010", year_end = "2015")


## ------------------------------------------------------------------------
current_conditions_fields("field_test", "metar-mesonet")


## ------------------------------------------------------------------------

agronomic_values_fields("field_test", day_start = "2016-07-01", day_end = "2016-07-31", accumulation_start_date = "2016-06-01", gdd_method = "modifiedstandard", gdd_base_temp = "10", gdd_min_boundary = "10", gdd_max_boundary = "30")


## ------------------------------------------------------------------------
agronomic_norms_fields("field_test", month_day_start = "07-01", month_day_end = "07-31", year_start = "2010", year_end = "2015", gdd_method = "modifiedstandard")


