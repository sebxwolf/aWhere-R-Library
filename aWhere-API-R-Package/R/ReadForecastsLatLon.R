#' @title GetForecastsLatLon.
#'
#' @description
#' \code{GetForecastsLatLon} calls Forecast Weather by Geolocation Endpoint of API using Lat/Lon Constuct
#'
#' @details
#' The Weather APIs provide access to aWhere's agriculture-specific Weather Terrain™ system,
#' and allows retrieval and integration of data across all different time ranges—long term normals,
#' daily observed, current weather, and forecasts. These APIs are designed for efficiency,
#' allowing you to customize the responses to return just the attributes you need.
#'
#' Use this API to return today's forecast plus the forecast for up to 7 more days. Forecasts are available
#' in many sizes of hourly blocks, from hourly to daily. The data this function returns is
#' Min/Max Temperature, Precipitation Amount, Chance of Precipitation,
#' Max Humidity, Relative Humidity, Solar Radiation, Average Wind Speed, Max Windspeed, Percentage
#' of Sky Covered by Clouds, and Percentage of Clear Sky.  Used the Lat/Lon construct to request data.
#' Uses default units returned by the API
#'
#' @references http://developer.awhere.com/api/reference/weather/forecasts/geolocation
#'
#' @param - latitude: the latitude of the requested location
#' @param - longitude: the longitude of the requested locations
#' @param - dayStart: character string of start date in form: YYYY-MM-DD
#'                    Defaults to system date if left blank
#' @param - dayEnd: character string of end date in form: YYYY-MM-DD
#'                  Defaults to system date + 7 if left blank
#' @param - blockSize: Integer value that corresponds to the number of hours to include in each time block.
#'                     Defaults to a 1 hour block.  This number must divide evenly into 24
#' @return data.table of requested data for dates requested
#'
#' @import httr
#' @import data.table
#' @import lubridate
#' @import jsonlite
#'
#' @examples
#' GetForecastsLatLon('39.8282', '-98.5795','2016-01-19','2016-01-24',12)

#' @export


GetForecastsLatLon <- function(latitude, longitude, dayStart = Sys.Date(), dayEnd = '',
                                   blockSize = 1) {

  if (exists('awhereEnv75247') == FALSE) {
    warning('Please Run the Command \'GetAccessToken()\' and then retry running command. \n')
    return()
  }

  if (exists('uid', envir = awhereEnv75247) == FALSE |
      exists('secret', envir = awhereEnv75247) == FALSE |
      exists('token', envir = awhereEnv75247) == FALSE) {
    warning('Please Run the Command \'getAccessToken()\' and then retry running command. \n')
    return()
  }

  if (suppressWarnings(is.na(as.double(latitude))) == FALSE) {
    if ((as.double(latitude) >= -90 & as.double(latitude) <= 90) == FALSE) {
      warning('The entered Latitude Value is not valid. Please correct\n')
      return()
    }
  } else {
    warning('The entered Latitude Value is not valid. Please correct\n')
    return()
  }

  if (suppressWarnings(is.na(as.double(longitude))) == FALSE) {
    if ((as.double(longitude) >= -180 & as.double(longitude) <= 180) == FALSE) {
      warning('The entered Longitude Value is not valid. Please correct\n')
      return()
    }
  } else {
    warning('The entered Longitude Value is not valid. Please correct\n')
    return()
  }

  if (suppressWarnings(is.na(ymd(dayStart))) == TRUE) {
    warning('The Start Date is Not Properly Formatted.  Please change to proper format. \n')
    return()
  }

  if ((dayEnd != '') == TRUE) {
    if (suppressWarnings(is.na(ymd(dayEnd))) == TRUE) {
      warning('The End Date is Not Properly Formatted.  Please change to proper format. \n')
      return()
    } else if (ymd(dayEnd) <= ymd(Sys.Date()) - days(1)) {
      warning('By default, this function can only be used to access data up until yesterday. \n
              Use the GetForecast function to request data from today onward.\n')
      return()
    }

    if (ymd(dayStart) > ymd(dayEnd)) {
      warning('DayStart must come before dayEnd.  Please correct \n')
      return()
    }
  }

  if (ymd(dayStart) < ymd(Sys.Date())) {
    warning('By default, this function can only be used to access data from today onward. \n
            Use the GetWeatherObservationsHist function to request data from yesterday backwards.\n')
    return()
  }

  if (ymd(dayStart) > ymd(Sys.Date()) + days(8)) {
    warning('By default, the aWhere APIs only allows forecast to be retrieved less than 8 days into the future. \n')
    return()
  }

  if ((24 %% blockSize) == 0){
    warning('The block size must divide evenly into 24. Please correct\n')
    return()
  }


  urlAddress <- "https://api.awhere.com/v2/weather"

  strBeg <- paste0('/locations')
  strCoord <- paste0('/',latitude,',',longitude)
  strType <- paste0('/forecasts')
  strDates <- paste0('/',dayStart,',',dayEnd)
  blockString <- paste0('?blockSize=',blockSize)


  address <- paste0(urlAddress, strBeg, strCoord, strType, strDates,blockString)

  doWeatherGet <- TRUE
  while (doWeatherGet == TRUE) {
    requestString <- paste0('request <- GET(address,
      	                                    add_headers(Authorization =
      	                                    paste0(\"Bearer \", awhereEnv75247$token)))')

    # Make forecast request

    eval(parse(text = requestString))

    a <- content(request, as = "text")

    #The JSONLITE Serializer propely handles the JSON conversion

    x <- jsonlite::fromJSON(a,flatten = TRUE)

    if (grepl('API Access Expired',a)) {
      GetAccessToken(awhereEnv75247$uid,awhereEnv75247$secret)
    } else {
      doWeatherGet <- FALSE
    }
  }
  dataTemp <- as.data.table(x[[1]])
  data <- as.data.table(rbindlist(dataTemp$forecast))
  varNames <- colnames(data)

  #This removes the non-data info returned with the JSON object
  data[,grep('.units',varNames) := NULL, with = FALSE]

#   varNames <- colnames(data)
#
#   for (x in 1:length(varNames)) {
#     if (varNames[x] == 'temperatures.max'){
#       varNames[x] <- 'maxTemperature'
#     } else if (varNames[x] == 'temperatures.min'){
#       varNames[x] <- 'minTemperature'
#     } else if (varNames[x] == 'precipitation.amount'){
#       varNames[x] <- 'precipitationAmount'
#     } else if (varNames[x] == 'precipitation.chance'){
#       varNames[x] <- 'chancePrecipitation'
#     } else if (varNames[x] == 'solar.amount'){
#       varNames[x] <- 'solarEnergy'
#     } else if (varNames[x] == 'relativeHumidity.max'){
#       varNames[x] <- 'maxRH'
#     } else if (varNames[x] == 'relativeHumidity.average'){
#       varNames[x] <- 'avgRH'
#     } else if (varNames[x] == 'wind.average'){
#       varNames[x] <- 'avgWind'
#     } else if (varNames[x] == 'wind.max'){
#       varNames[x] <- 'maxWind'
#     } else if (varNames[x] == 'sky.cloudCover'){
#       varNames[x] <- 'pctCloudCover'
#     }  else if (varNames[x] == 'sky.sunshine'){
#       varNames[x] <- 'pctSunshine'
#     }
#   }
#
#   setnames(data,varNames)


  return(data)
}
