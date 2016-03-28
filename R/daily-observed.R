#' @title daily_observed_fields.
#'
#' @description
#' \code{daily_observed_fields} calls Daily Observed Weather Endpoint of API using Field Location Construct
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
#' Morning Max Windspeed, and Average Windspeed.  Uses the Fields Name construct for requesting data.
#' Uses default units returned by the API
#'
#' @references http://developer.awhere.com/api/reference/weather/observations
#'
#' @param - latitude: the latitude of the requested location
#' @param - longitude: the longitude of the requested locations
#' @param - day_start: character string of start date in form: YYYY-MM-DD
#'                    Defaults to system date -1 if left blank
#' @param - day_end: character string of end date in form: YYYY-MM-DD
#'                  If Not included will return data only for start date
#' @return data.table of requested data for dates requested
#'
#' @import httr
#' @import data.table
#' @import lubridate
#' @import jsonlite
#'
#' @examples
#' daily_observed_fields('field123','2015-04-28','2015-05-01')

#' @export


daily_observed_fields <- function(field_id,
                                       day_start = '', day_end = '') {

  if (exists('awhereEnv75247') == FALSE) {
    warning('Please Run the Command \'get_token()\' and then retry running command. \n')
    return()
  }

  if (exists('uid', envir = awhereEnv75247) == FALSE |
      exists('secret', envir = awhereEnv75247) == FALSE |
      exists('token', envir = awhereEnv75247) == FALSE) {
    warning('Please Run the Command \'get_token()\' and then retry running command. \n')
    return()
  }

  if (day_start == '' & day_end != '') {
    warning('If day_end is specified so must day_start.  Please correct \n')
    return()
  }

  currentFields <- GetFields(field_id)
  if ((field_id %in% currentFields$field_id) == FALSE) {
    warning('The Provided field name is not a field currently associated with your account. \n
            Please create the field before proceeding. \n')
    return()
  }

  if (day_start != '') {
    if (suppressWarnings(is.na(ymd(day_start))) == TRUE) {
      warning('The Start Date is Not Properly Formatted.  Please change to proper format. \n')
      return()
    } #else if (ymd(day_start) <= ymd(Sys.Date())-months(30)) {
      #warning('By default, the aWhere APIs only allow daily data from the previous 30 months. \n
      #       Use the Norms API for long-term averages or speak to your account manager for longer access.\n')
      #return()
    # }
  }

  if ((day_end != '') == TRUE) {
    if (suppressWarnings(is.na(ymd(day_end))) == TRUE) {
      warning('The End Date is Not Properly Formatted.  Please change to proper format. \n')
      return()
    } else if (ymd(day_end) > ymd(Sys.Date()) - days(1)) {
      warning('By default, this function can only be used to access data up until yesterday. \n
              Use the GetForecast function to request data from today onward.\n')
      return()
    }
  }

  if (day_start !='' & day_end != '') {
    if (ymd(day_start) > ymd(day_end)) {
      warning('The endDate must come after the startDate. Please correct\n')
      return()
    }
  }

  ## Create Request
  #Calculate number of loops needed if requesting more than 50 days
  numObsReturned <- 50

  if (day_start != '' & day_end != '') {
    numOfDays <- as.numeric(difftime(ymd(day_end), ymd(day_start), units = 'days'))
    allDates <- seq(as.Date(ymd(day_start)),as.Date(ymd(day_end)), by="days")

    loops <- ((length(allDates))) %/% numObsReturned
    remainder <- ((length(allDates))) %% numObsReturned

  } else if (day_start != '') {

    numOfDays <- 1
    allDates <- ymd(day_start)
    loops <- 1
    remainder <- 0
  } else {
    numOfDays <- 1
    allDates <- ''
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

    if(paste(allDates,sep = '',collapse ='') != '') {
      day_start <- allDates[starting]
      day_end <- allDates[ending]
      if(is.na(day_end)) {
        tempDates <- allDates[c(starting:length(allDates))]
        day_start <- tempDates[1]
        day_end <- tempDates[length(tempDates)]
      }
    }


    # Create query

    urlAddress <- "https://api.awhere.com/v2/weather"

    strBeg <- paste0('/fields')
    strCoord <- paste0('/',field_id)
    strType <- paste0('/observations')

    if(paste(allDates,sep = '',collapse ='') != '') {
      strDates <- paste0('/',day_start,',',day_end)

      returnedAmount <- as.integer(difftime(ymd(day_end),ymd(day_start),units = 'days')) + 1L
      if (returnedAmount > numObsReturned) {
        returnedAmount <- numObsReturned
      }
      limitString <- paste0('?limit=',returnedAmount)

    } else {
      strDates <- ''
      limitString <- paste0('?limit=50')
    }

    address <- paste0(urlAddress, strBeg, strCoord, strType, strDates, limitString)

    doWeatherGet <- TRUE

    while (doWeatherGet == TRUE) {

      requestString <- paste0('request <- GET(address,
  	                                    add_headers(Authorization =
  	                                    paste0(\"Bearer \", awhereEnv75247$token)))')

      # Make request

      eval(parse(text = requestString))

      a <- suppressMessages(content(request, as = "text"))

      #The JSONLITE Serializer properly handles the JSON conversion

      x <- jsonlite::fromJSON(a,flatten = TRUE)

      if (grepl('API Access Expired',a)) {
        get_token(awhereEnv75247$uid,awhereEnv75247$secret)
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
#       } else if (varNames[x] == 'location.field_id') {
#         varNames[x] <- 'field_id'
#       }
#     }

#    setnames(data,varNames)

    dataList[[i]] <- data

  }


  allWeath <- rbindlist(dataList)
  setkey(allWeath,date)

  return(as.data.frame(allWeath))
}


#' @title daily_observed_latlng.
#'
#' @description
#' \code{daily_observed_latlng} calls Daily Observed Weather by Geolocation Endpoint of API using Lat/Lon
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
#' Morning Max Windspeed, and Average Windspeed.  Uses the lat/lon construct for requesting data.
#' Uses default units returned by the API
#'
#' @references http://developer.awhere.com/api/reference/weather/observations/geolocation
#'
#' @param - latitude: the latitude of the requested location
#' @param - longitude: the longitude of the requested locations
#' @param - day_start: character string of start date in form: YYYY-MM-DD
#'                    Defaults to system date -1 if left blank
#' @param - day_end: character string of end date in form: YYYY-MM-DD
#'                  If Not included will return data only for start date
#' @return data.table of requested data for dates requested
#'
#' @import httr
#' @import data.table
#' @import lubridate
#' @import jsonlite
#'
#' @examples
#' daily_observed_latlng('39.8282', '-98.5795','2015-04-28','2015-05-01')

#' @export


daily_observed_latlng <- function(latitude, longitude,
                                          day_start = as.character(ymd(Sys.Date()) - days(1)), day_end = '') {

  if (exists('awhereEnv75247') == FALSE) {
    warning('Please Run the Command \'get_token()\' and then retry running command. \n')
    return()
  }

  if (exists('uid', envir = awhereEnv75247) == FALSE |
      exists('secret', envir = awhereEnv75247) == FALSE |
      exists('token', envir = awhereEnv75247) == FALSE) {
    warning('Please Run the Command \'get_token()\' and then retry running command. \n')
    return()
  }

  if (day_end != '') {
    if (ymd(day_start) > ymd(day_end)) {
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

  if (suppressWarnings(is.na(ymd(day_start))) == TRUE) {
    warning('The Start Date is Not Properly Formatted.  Please change to proper format. \n')
    return()
  }

  if ((day_end != '') == TRUE) {
    if (suppressWarnings(is.na(ymd(day_end))) == TRUE) {
      warning('The End Date is Not Properly Formatted.  Please change to proper format. \n')
      return()
    } else if (ymd(day_end) > ymd(Sys.Date()) - days(1)) {
      warning('By default, this function can only be used to access data up until yesterday. \n
              Use the GetForecast function to request data from today onward.\n')
      return()
    }
  }

  #  if (ymd(day_start) <= ymd(Sys.Date())-months(30)) {
  #    warning('By default, the aWhere APIs only allow daily data from the previous 30 months. \n
  #             Use the Norms API for long-term averages or speak to your account manager for longer access.\n')
  #    return()
  #  }

  ## Create Request
  #Calculate number of loops needed if requesting more than 50 days
  numObsReturned <- 50

  if (day_end != '') {
    numOfDays <- as.numeric(difftime(ymd(day_end), ymd(day_start), units = 'days'))
    allDates <- seq(as.Date(ymd(day_start)),as.Date(ymd(day_end)), by="days")

    loops <- ((length(allDates))) %/% numObsReturned
    remainder <- ((length(allDates))) %% numObsReturned

  } else {

    numOfDays <- 1
    allDates <- ymd(day_start)
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
    day_start <- allDates[starting]
    day_end <- allDates[ending]
    if(is.na(day_end)) {
      tempDates <- allDates[c(starting:length(allDates))]
      day_start <- tempDates[1]
      day_end <- tempDates[length(tempDates)]
    }


    # Create query

    urlAddress <- "https://api.awhere.com/v2/weather"

    strBeg <- paste0('/locations')
    strCoord <- paste0('/',latitude,',',longitude)
    strType <- paste0('/observations')
    strDates <- paste0('/',day_start,',',day_end)


    returnedAmount <- as.integer(difftime(ymd(day_end),ymd(day_start),units = 'days')) + 1L
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

      a <- suppressMessages(content(request, as = "text"))

      #The JSONLITE Serializer properly handles the JSON conversion

      x <- jsonlite::fromJSON(a,flatten = TRUE)

      if (grepl('API Access Expired',a)) {
        get_token(awhereEnv75247$uid,awhereEnv75247$secret)
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
    #       } else if (varNames[x] == 'location.field_id') {
    #         varNames[x] <- 'field_id'
    #       }
    #     }

    #    setnames(data,varNames)

    dataList[[i]] <- data

  }


  allWeath <- rbindlist(dataList)
  setkey(allWeath,date)

  return(as.data.frame(allWeath))
  }
