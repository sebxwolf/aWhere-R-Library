#' @title forecasts_fields
#'
#' @description
#' \code{forecasts_fields} pulls forecast weather data from aWhere's API based on field id
#'
#' @details
#' This function returns today's forecast plus the forecast for up to 7 more days. Forecasts are available
#' in many sizes of time blocks, from hourly to daily. The data this function returns is
#' Min/Max Temperature, Precipitation Amount, Chance of Precipitation,
#' Max Humidity, Relative Humidity, Solar Radiation, Average Wind Speed, Max Windspeed, Percentage
#' of Sky Covered by Clouds, and Percentage of Clear Sky for the field id specified.
#' Default units are returned by the API.
#'
#' Note that when block_size = 1 the fields min/max relative humidity and min/max wind will be NA
#'
#' The Weather APIs provide access to aWhere's agriculture-specific Weather Terrain system,
#' and allows retrieval and integration of data across all different time ranges long term normals,
#' daily observed, current weather, and forecasts. These APIs are designed for efficiency,
#' allowing you to customize the responses to return just the attributes you need.
#'
#' Note about dates: The system does not adjust for any difference in dates between the location of the user
#'           and where data is being requested from.  It is the responsibility of the user to ensure a valid
#'           date range is specified given any differences in timezone.  These differences can have implications
#'           for whether a given date should be requested from the daily_observed functions or the forecast functions
#'
#' @references http://developer.awhere.com/api/forecast-weather-api
#'
#' @param - field_id: the field_id associated with the location for which you want to pull data.
#' Field IDs are created using the create_field function. (string)
#' @param - day_start: character string of the first day for which you want to retrieve data, in the form: YYYY-MM-DD
#'                    Defaults to system date if left blank. (optional)
#' @param - day_end: character string of the last day for which you want to retrieve data, in form: YYYY-MM-DD
#'                  Defaults to system date + 7 if left blank. (optional)
#' @param - block_size: Integer value that corresponds to the number of hours to include in each time block.
#'                     Defaults to a 1 hour block.  This value must divide evenly into 24. (integer - optional)
#' @param - useLocalTime: whether the data specified is the date specified at the location where data is
#'                        being requested from or at UTC = 0.  Default is TRUE
#' @param - keyToUse: aWhere API key to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#' @param - secretToUse: aWhere API secret to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#' @param - tokenToUse: aWhere API token to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#'
#' @import httr
#' @import data.table
#' @import lubridate
#' @import jsonlite
#'
#' @return data.frame of requested data for dates requested
#'
#' @examples
#' \dontrun{forecasts_fields(field_id = 'field_test', day_start = as.character(Sys.Date()), block_size = 12)
#'          forecasts_fields('field_test', day_start = as.character(Sys.Date()), day_end = as.character(Sys.Date() + 5))}
#' @export


forecasts_fields <- function(field_id
                             ,day_start = as.character(Sys.Date())
                             ,day_end = ''
                             ,block_size = 1
                             ,useLocalTime = TRUE
                             ,keyToUse = awhereEnv75247$uid
                             ,secretToUse = awhereEnv75247$secret
                             ,tokenToUse = awhereEnv75247$token) {

  checkCredentials(keyToUse,secretToUse,tokenToUse)
  checkValidField(field_id,keyToUse,secretToUse,tokenToUse)
  checkValidStartEndDatesForecast(day_start,day_end)
  checkForecastParams(day_start,block_size)

  #Create Query
  urlAddress <- "https://api.awhere.com/v2/weather"

  strBeg <- paste0('/fields')
  strCoord <- paste0('/',field_id)
  strType <- paste0('/forecasts')

  if (as.character(day_start) != '' & as.character(day_end) != '') {
    strDates <- paste0('/',day_start,',',day_end)
  } else if (as.character(day_start) != '') {
    strDates <- paste0('/',day_start,',',day_start)
  } else {
    strDates <- ''
  }

  blockString <- paste0('?blockSize=',block_size)
  localTimeString <- paste0('&useLocalTime=',useLocalTime)

  url <- paste0(urlAddress, strBeg, strCoord, strType, strDates,blockString,localTimeString)

  doWeatherGet <- TRUE
  while (doWeatherGet == TRUE) {
    postbody = ''
    request <- httr::GET(url, body = postbody, httr::content_type('application/json'),
                         httr::add_headers(Authorization =paste0("Bearer ", tokenToUse)))

    a <- suppressMessages(httr::content(request, as = "text"))

    doWeatherGet <- check_JSON(a,request)
  }

  #The JSONLITE Serializer properly handles the JSON conversion
  x <- jsonlite::fromJSON(a,flatten = TRUE)

  if (length(x) != 4) {
    dataTemp <- x[[1]]$forecast
    data <- data.table::as.data.table(data.table::rbindlist(dataTemp))
  } else { #this corresponds to a single day of data
    data <- data.table::as.data.table(x[[3]])
  }

  varNames <- colnames(data)

  #This removes the non-data info returned with the JSON object
  data[,grep('.units',varNames) := NULL]

  data[,c('soilTemperatures','soilMoisture') := NULL]

  currentNames <- data.table::copy(colnames(data))
  data[,field_id  := field_id]
  data.table::setcolorder(data,c('field_id',currentNames))

  if(block_size == 1) {
    data[,c("relativeHumidity.max", "relativeHumidity.min", "wind.max", "wind.min") := NULL]
  }

  checkDataReturn_forecasts(data,day_start,day_end,block_size)

  return(as.data.frame(data))
}

#' @title forecasts_latlng
#'
#' @description
#' \code{forecasts_latlng} pulls historical weather data from aWhere's API based on latitude & longitude
#'
#' @details
#' This function returns today's forecast plus the forecast for up to 7 more days. Forecasts are available
#' in many sizes of hourly blocks, from hourly to daily. The data this function returns is
#' Min/Max Temperature, Precipitation Amount, Chance of Precipitation,
#' Max Humidity, Relative Humidity, Solar Radiation, Average Wind Speed, Max Windspeed, Percentage
#' of Sky Covered by Clouds, and Percentage of Clear Sky for the location specified by latitude and longitude.
#' Default units are returned by the API. Latitude and longitude must be in decimal degrees.
#'
#' Note that when block_size = 1 the fields min/max relative humidity and min/max wind will be NA
#'
#' The Weather APIs provide access to aWhere's agriculture-specific Weather Terrain system,
#' and allows retrieval and integration of data across all different time ranges long term normals,
#' daily observed, current weather, and forecasts. These APIs are designed for efficiency,
#' allowing you to customize the responses to return just the attributes you need.
#'
#' Note about dates: The system does not adjust for any difference in dates between the location of the user
#'           and where data is being requested from.  It is the responsibility of the user to ensure a valid
#'           date range is specified given any differences in timezone.  These differences can have implications
#'           for whether a given date should be requested from the daily_observed functions or the forecast functions
#'
#' @references http://developer.awhere.com/api/reference/weather/forecasts/geolocation
#'
#' @param - latitude: the latitude of the requested location (double)
#' @param - longitude: the longitude of the requested locations (double)
#' @param - day_start: character string of the first day for which you want to retrieve data, in the form: YYYY-MM-DD
#'                    Defaults to system date if left blank. (optional)
#' @param - day_end: character string of the last day for which you want to retrieve data, in form: YYYY-MM-DD
#'                  Defaults to system date + 7 if left blank. (optional)
#' @param - block_size: Integer value that corresponds to the number of hours to include in each time block.
#'                     Defaults to a 1 hour block.  This value must divide evenly into 24. (integer - optional)
#' @param - useLocalTime: whether the data specified is the date specified at the location where data is
#'                        being requested from or at UTC = 0.  Default is TRUE
#' @param - keyToUse: aWhere API key to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#' @param - secretToUse: aWhere API secret to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#' @param - tokenToUse: aWhere API token to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#'
#' @return data.frame of requested data for dates requested
#'
#' @import httr
#' @import data.table
#' @import lubridate
#' @import jsonlite
#'
#' @examples
#' \dontrun{forecasts_latlng(39.8282, -98.5795,as.character(Sys.Date()),as.character(Sys.Date() + 5), block_size = 12)
#'          forecasts_latlng(19.328489, -99.145681, day_start = as.character(Sys.Date()), block_size = 4)}

#' @export


forecasts_latlng <- function(latitude
                             ,longitude
                             ,day_start = as.character(Sys.Date())
                             ,day_end = ''
                             ,block_size = 1
                             ,useLocalTime = TRUE
                             ,keyToUse = awhereEnv75247$uid
                             ,secretToUse = awhereEnv75247$secret
                             ,tokenToUse = awhereEnv75247$token) {

  checkCredentials(keyToUse,secretToUse,tokenToUse)
  checkValidLatLong(latitude,longitude)
  checkValidStartEndDatesForecast(day_start,day_end)
  checkForecastParams(day_start,block_size)

  #Create Query
  urlAddress <- "https://api.awhere.com/v2/weather"

  strBeg <- paste0('/locations')
  strCoord <- paste0('/',latitude,',',longitude)
  strType <- paste0('/forecasts')
  strDates <- paste0('/',day_start,',',day_end)
  blockString <- paste0('?blockSize=',block_size)
  localTimeString <- paste0('&useLocalTime=',useLocalTime)

  url <- paste0(urlAddress, strBeg, strCoord, strType, strDates,blockString,localTimeString)

  doWeatherGet <- TRUE
  while (doWeatherGet == TRUE) {
    postbody = ''
    request <- httr::GET(url, body = postbody, httr::content_type('application/json'),
                         httr::add_headers(Authorization =paste0("Bearer ", tokenToUse)))

    # Make forecast request
    a <- suppressMessages(httr::content(request, as = "text"))

    doWeatherGet <- check_JSON(a,request)
  }

  #The JSONLITE Serializer properly handles the JSON conversion
  x <- jsonlite::fromJSON(a,flatten = TRUE)

  if (length(x) != 4) {
    dataTemp <- x[[1]]$forecast
    data <- data.table::as.data.table(data.table::rbindlist(dataTemp))
  } else { #this corresponds to a single day of data
    data <- data.table::as.data.table(x[[3]])
  }

  varNames <- colnames(data)

  #This removes the non-data info returned with the JSON object
  data[,grep('.units',varNames) := NULL]

  data[,c('soilTemperatures','soilMoisture') := NULL]

  currentNames <- data.table::copy(colnames(data))
  data[,latitude  := latitude]
  data[,longitude := longitude]
  data.table::setcolorder(data,c('latitude','longitude',currentNames))

  if(block_size == 1) {
    data[,c("relativeHumidity.max", "relativeHumidity.min", "wind.max", "wind.min") := NULL]
  }

  checkDataReturn_forecasts(data,day_start,day_end,block_size)

  return(as.data.frame(data))
}

