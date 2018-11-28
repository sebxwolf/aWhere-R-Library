## ----setup, include=FALSE-------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,
                      message = FALSE,
                      warning = FALSE,
                      results='hide')
options(width = 100)

## ----getData, include = FALSE---------------------------------------------------------------------
library(aWhereAPI)
aWhereAPI::load_credentials("credentials_trial.txt")

obs_startdate <- as.character(Sys.Date() - 30)
obs_enddate <- as.character(Sys.Date() - 1)
for_startdate <- as.character(Sys.Date())
for_enddate <- as.character(Sys.Date() + 7)

try(aWhereAPI::delete_field(field_id = "1"),silent = TRUE)
try(aWhereAPI::delete_field(field_id = "2"),silent = TRUE)

## ----loadPackage----------------------------------------------------------------------------------
library(aWhereAPI)

## ----getToken1------------------------------------------------------------------------------------
aWhereAPI::load_credentials("credentials_trial.txt")

## ----getToken2, eval = FALSE----------------------------------------------------------------------
#  aWhereAPI::get_token(uid = "ABCDEFG", secret = "123456")

## ----createField----------------------------------------------------------------------------------
aWhereAPI::create_field(field_id = "1", latitude = 40, 
                        longitude = -90, farm_id = "My Farm", 
                        field_name = "Field 1", acres = 30)

aWhereAPI::create_field(field_id = "2", latitude = 40, 
                        longitude = -90, farm_id = "My Other Farm", 
                        field_name = "Field A", acres = 35)

## ----getFields, results=TRUE----------------------------------------------------------------------
aWhereAPI::get_fields()
aWhereAPI::get_fields(field_id = "1")

## ----updateField, results=TRUE--------------------------------------------------------------------
aWhereAPI::update_field(field_id = "1", 
                        variable_update = "name", 
                        value_update = "Old Field 1")
aWhereAPI::get_fields(field_id = "1")


## ----deleteField, results=TRUE--------------------------------------------------------------------
aWhereAPI::delete_field(field_id = "1")
aWhereAPI::get_fields()

## ----getObsWeather, results=TRUE------------------------------------------------------------------

obs_startdate <- as.character(Sys.Date() - 30)
obs_enddate <- as.character(Sys.Date() - 1)

observed <- aWhereAPI::daily_observed_latlng(latitude = 40, 
                                             longitude = -90, 
                                             day_start = obs_startdate, 
                                             day_end = obs_enddate)

# take a look at the first 6 rows of location, date, and temperature data
head(observed[,c('latitude', 'longitude', 'date',
                 'temperatures.min','temperatures.max')])

## ----getDailyForecast, results=TRUE---------------------------------------------------------------
for_startdate <- as.character(Sys.Date())
for_enddate <- as.character(Sys.Date() + 7)

dailyforecast <- aWhereAPI::forecasts_latlng(latitude = 40, 
                                             longitude = -90, 
                                             day_start = for_startdate, 
                                             day_end = for_enddate, 
                                             block_size = 24)

# take a look at the first 6 rows of the daily forecast location,
# starting/ending times, and sky cloud cover data 
head(dailyforecast[,c('latitude','longitude',
                      'startTime','endTime','sky.cloudCover')])

## ----getHourlyForecast, results=TRUE--------------------------------------------------------------
hourlyforecast <- aWhereAPI::forecasts_latlng(latitude = 40, 
                                              longitude = -90, 
                                              day_start = for_startdate, 
                                              day_end = for_enddate, 
                                              block_size = 1)

# take a look at the first 6 rows of the hourly forecast location,
# starting/ending times, and sky cloud cover data 
head(hourlyforecast[,c('latitude','longitude',
                      'startTime','endTime','sky.cloudCover')])

## ----getNorms, results=TRUE-----------------------------------------------------------------------
norms <- aWhereAPI::weather_norms_latlng(latitude = 40, 
                                         longitude = -90, 
                                         monthday_start = "06-01", 
                                         monthday_end = "06-30", 
                                         year_start = 2006, 
                                         year_end = 2017)

# take a look at the first 6 rows of the long-term normal data
# location, date, max precipitation, and average precipitation
head(norms[,c('latitude','longitude','day',
              'maxTemp.average','precipitation.average')])

## ----seeData, results=TRUE------------------------------------------------------------------------
# take a look at the first 6 rows of location, date, and temperature data
head(observed[,c('latitude', 'longitude', 'date',
                 'temperatures.min','temperatures.max')])

## ----writeCSV-------------------------------------------------------------------------------------
write.csv(observed, file = "testFile.csv", row.names = F)

## ----readCSV--------------------------------------------------------------------------------------
obs_csv <- read.csv(file = "testFile.csv", header = T, 
                    stringsAsFactors = FALSE)

## ----equalCSV, results=TRUE-----------------------------------------------------------------------

# take a look at the first 6 rows of location, date, and temperature 

# for the orignal data frame
head(observed[,c('latitude', 'longitude', 'date',
                 'temperatures.min','temperatures.max')])

# for the CSV data 
head(obs_csv[,c('latitude', 'longitude', 'date',
                 'temperatures.min','temperatures.max')])

## ----allEqual, results=TRUE-----------------------------------------------------------------------
all.equal(observed, obs_csv)

