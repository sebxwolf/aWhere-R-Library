#' @title GetDailyObservedWeatherLatLon.
#'
#' @description
#' \code{GetDailyObservedWeatherLatLon} calls Daily Observed Weather by Geolocation Endpoint of API using Lat/Lon Constuct
#'
#' @details
#' The Weather APIs provide access to aWhere's agriculture-specific Weather Terrain™ system,
#' and allows retrieval and integration of data across all different time ranges—long term normals,
#' daily observed, current weather, and forecasts. These APIs are designed for efficiency,
#' allowing you to customize the responses to return just the attributes you need.
#'
#' Understanding the recent and long-term daily weather is critical for making in-season decisions.
#' This API opens the weather attributes that matter most to agriculture. By default you are allowed
#' access of up to 30 months of data (beyond that, use the Norms API to get multi-year averages).
#' This function assumes that the data to be requested is Min/Max Temperature, Precipitation,
#' Min/Max Humidity, Solar Radiation, and Maximum Wind Speed,
#' Morning Max Windspeed, and Average Windspeed.  Uses the lat/lon construct for requestion data.
#' Uses default units returned by the API
#'
#' @references http://developer.awhere.com/api/reference/weather/observations/geolocation
#'
#' @param - latitude: the latitude of the requested location
#' @param - longitude: the longitude of the requested locations
#' @param - dayStart: character string of start date in form: YYYY-MM-DD
#'                    Defaults to system date -1 if left blank
#' @param - dayEnd: character string of end date in form: YYYY-MM-DD
#'                  If Not included will return data only for start date
#' @return data.table of requested data for dates requested
#'
#' @import httr
#' @import data.table
#' @import lubridate
#' @import jsonlite
#'
#' @examples
#' GetDailyObservedWeatherLatLon('39.8282', '-98.5795','2015-04-28','2015-05-01')

#' @export


GetDailyObservedWeatherLatLon <- function(latitude, longitude,
                                       dayStart = as.character(ymd(Sys.Date()) - days(1)), dayEnd = '') {

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

  if (dayEnd != '') {
    if (ymd(dayStart) > ymd(dayEnd)) {
      warning('The endDate must come after the startDate. Please correct\n')
      return()
    }
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
    } else if (ymd(dayEnd) > ymd(Sys.Date()) - days(1)) {
      warning('By default, this function can only be used to access data up until yesterday. \n
              Use the GetForecast function to request data from today onward.\n')
      return()
    }
  }

  if (ymd(dayStart) <= ymd(Sys.Date())-months(30)) {
    warning('By default, the aWhere APIs only allow daily data from the previous 30 months. \n
             Use the Norms API for long-term averages or speak to your account manager for longer access.\n')
    return()
  }

  ## Create Request
  #Calculate number of loops needed if requesting more than 50 days
  numObsReturned <- 50

  if (dayEnd != '') {
    numOfDays <- as.numeric(difftime(ymd(dayEnd), ymd(dayStart), units = 'days'))
    allDates <- seq(as.Date(ymd(dayStart)),as.Date(ymd(dayEnd)), by="days")

    loops <- ((length(allDates))) %/% numObsReturned
    remainder <- ((length(allDates))) %% numObsReturned

  } else {

    numOfDays <- 1
    allDates <- ymd(dayStart)
    loops <- 1
    remainder <- 0
  }

  if(remainder > 0) {
    loops <- loops + 1
  }
  i <- 1

  dataList <- list()

  # loop through, making requests in 50-day chunks

  for (i in 1:loops) {

    starting = numObsReturned*(i-1)+1
    ending = numObsReturned*i
    dayStart <- allDates[starting]
    dayEnd <- allDates[ending]
    if(is.na(dayEnd)) {
      tempDates <- allDates[c(starting:length(allDates))]
      dayStart <- tempDates[1]
      dayEnd <- tempDates[length(tempDates)]
    }


    # Create query

    urlAddress <- "https://api.awhere.com/v2/weather"

    strBeg <- paste0('/locations')
    strCoord <- paste0('/',latitude,',',longitude)
    strType <- paste0('/observations')
    strDates <- paste0('/',dayStart,',',dayEnd)


    returnedAmount <- as.integer(difftime(ymd(dayEnd),ymd(dayStart),units = 'days')) + 1L
    if (returnedAmount > numObsReturned) {
      returnedAmount <- numObsReturned
    }
    limitString <- paste0('?limit=',returnedAmount)

    address <- paste0(urlAddress, strBeg, strCoord, strType, strDates, limitString)

    doWeatherGet <- TRUE

    while (doWeatherGet == TRUE) {

      requestString <- paste0('request <- GET(address,
  	                                    add_headers(Authorization =
  	                                    paste0(\"Bearer \", awhereEnv75247$token)))')

      # Make request

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

    data <- as.data.table(x[[1]])

    varNames <- colnames(data)

    #This removes the non-data info returned with the JSON object
    data[,grep('_links',varNames) := NULL, with = FALSE]
    data[,grep('.units',varNames) := NULL, with = FALSE]

#    varNames <- colnames(data)
#
#     for (x in 1:length(varNames)) {
#
#       if (varNames[x] == 'location.latitude'){
#         varNames[x] <- 'latitude'
#       } else if (varNames[x] == 'location.longitude'){
#         varNames[x] <- 'longitude'
#       } else if (varNames[x] == 'temperatures.max'){
#         varNames[x] <- 'maxTemperature'
#       } else if (varNames[x] == 'temperatures.min'){
#         varNames[x] <- 'minTemperature'
#       } else if (varNames[x] == 'precipitation.amount'){
#         varNames[x] <- 'precipitation'
#       } else if (varNames[x] == 'solar.amount'){
#         varNames[x] <- 'solarEnergy'
#       } else if (varNames[x] == 'relativeHumidity.max'){
#         varNames[x] <- 'maxRH'
#       } else if (varNames[x] == 'relativeHumidity.min'){
#         varNames[x] <- 'minRH'
#       } else if (varNames[x] == 'wind.morningMax'){
#         varNames[x] <- 'maxMorningWind'
#       } else if (varNames[x] == 'wind.dayMax'){
#         varNames[x] <- 'maxDayWind'
#       } else if (varNames[x] == 'wind.average'){
#         varNames[x] <- 'avgWind'
#       } else if (varNames[x] == 'location.fieldId') {
#         varNames[x] <- 'fieldId'
#       }
#     }

#    setnames(data,varNames)

    dataList[[i]] <- data

  }


  allWeath <- rbindlist(dataList)
  setkey(allWeath,date)

  return(allWeath)
}
