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
#' The Weather APIs provide access to aWhere's agriculture-specific Weather Terrain system,
#' and allows retrieval and integration of data across all different time ranges long term normals,
#' daily observed, current weather, and forecasts. These APIs are designed for efficiency,
#' allowing you to customize the responses to return just the attributes you need.
#'
#'
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

  url <- paste0(urlAddress, strBeg, strCoord, strType, strDates,blockString)

  doWeatherGet <- TRUE
  while (doWeatherGet == TRUE) {
    postbody = ''
    request <- httr::GET(url, body = postbody, httr::content_type('application/json'),
                         httr::add_headers(Authorization =paste0("Bearer ", awhereEnv75247$token)))

    a <- suppressMessages(content(request, as = "text"))

    if (grepl('API Access Expired',a)) {
      get_token(keyToUse,secretToUse)
    } else {
      checkStatusCode(request)
      doWeatherGet <- FALSE
    }
  }
  
  #The JSONLITE Serializer properly handles the JSON conversion
  x <- jsonlite::fromJSON(a,flatten = TRUE)
  
  if (length(x) != 4) {
    dataTemp <- x[[1]]$forecast
    data <- as.data.table(rbindlist(dataTemp))
  } else { #this corresponds to a single day of data
    data <- as.data.table(x[[3]])
  }

  varNames <- colnames(data)

  #This removes the non-data info returned with the JSON object
  data[,grep('.units',varNames) := NULL]

  #These variables are unique to the forecast API and have a different schema
  #than the rest.  For the time being remove
  data[,c('soilMoisture','soilTemperatures') := NULL]
  
  currentNames <- copy(colnames(data))
  data[,field_id  := field_id]
  setcolorder(data,c('field_id',currentNames))

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
#' The Weather APIs provide access to aWhere's agriculture-specific Weather Terrain system,
#' and allows retrieval and integration of data across all different time ranges long term normals,
#' daily observed, current weather, and forecasts. These APIs are designed for efficiency,
#' allowing you to customize the responses to return just the attributes you need.
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


  url <- paste0(urlAddress, strBeg, strCoord, strType, strDates,blockString)

  doWeatherGet <- TRUE
  while (doWeatherGet == TRUE) {
    postbody = ''
    request <- httr::GET(url, body = postbody, httr::content_type('application/json'),
                         httr::add_headers(Authorization =paste0("Bearer ", tokenToUse)))

    # Make forecast request
    a <- suppressMessages(content(request, as = "text"))

    if (grepl('API Access Expired',a)) {
      get_token(keyToUse,secretToUse)
    } else {
      checkStatusCode(request)
      doWeatherGet <- FALSE
    }
  }
  
  #The JSONLITE Serializer properly handles the JSON conversion
  x <- jsonlite::fromJSON(a,flatten = TRUE)

  if (length(x) != 4) {
    dataTemp <- x[[1]]$forecast
    data <- as.data.table(rbindlist(dataTemp))
  } else { #this corresponds to a single day of data
    data <- as.data.table(x[[3]])
  }

  varNames <- colnames(data)

  #This removes the non-data info returned with the JSON object
  data[,grep('.units',varNames) := NULL]

  #These variables are unique to the forecast API and have a different schema
  #than the rest.  For the time being remove
  data[,c('soilMoisture','soilTemperatures') := NULL]
  
  currentNames <- copy(colnames(data))
  data[,latitude  := latitude]
  data[,longitude := longitude]
  setcolorder(data,c('latitude','longitude',currentNames))

  checkDataReturn_forecasts(data,day_start,day_end,block_size)
  
  return(as.data.frame(data))
}

