#' @title GetAgronomicNormsLatLon.
#'
#' @description
#' \code{GetAgronomicNormsLatLon} calls Historic Agronomic Norms by Geolocation Endpoint of API using Lat/Lon Constuct
#'
#' @details
#' This is a flexible API that allows you to calculate the averages for agronomic values
#' and accumulations across any range of years for which we have data. Whereas the Agronomic
#' Values and Accumulations API only supports up to 30 months of daily data, this API allow
#' you to compare this year and the previous year to the long-term normals (however many years
#' you want to include).  Uses the aWhere API Lat/Lon construct to request data. Uses default
#' units returned by the API
#'
#' @references http://developer.awhere.com/api/reference/agronomics/norms/geolocation
#'
#' @param - latitude: the latitude for the location for which you want data
#' @param - longitude: the latitude for the location for which you want data
#' @param - monthDayStart: character string of the month and day for the start
#'                         of the range of days you are calculating norms for, e.g., '07-01' (July 1)
#' @param - monthDayEnd: character string of the month and day for the end of the
#'                       range of days you are calculating norms for, e.g., '07-10' (July 10)
#' @param - yearStart: the starting year (inclusive) of the range of years for which
#'                     you're calculating norms, as a string, e.g., '2008'
#' @param - yearStart: the end year (inclusive) of the range of years for which you're
#'                     calculating norms, as a string, e.g., '2015'
#' @param - excludeYears: You can opt to exclude one or more years from the range, and
#'                        it's values will not be included in the averages. To exclude
#'                        multiple years, separate them with a comma. Note: You must always have
#'                        at least three years of data to average
#' @param - accumulationStartDate: If you want to start counting accumulations from
#'                                 before the specified start date (or before the
#'                                 planting date if using the most recent Planting),
#'                                 use this parameter to specify the date from which
#'                                 you wish to start counting. The daily values object
#'                                 will still only return the days between the start
#'                                 and end date. This date must come before the start date.
#' @param - gddMethod: There are variety of equations available for calculating growing degree-days.
#'                     Valid entries are: 'standard', 'modifiedstandard', 'min-temp-cap', 'min-temp-constant'
#'                     See the API documentation for a description of each method.  The standard
#'                     method will be used if none is specified
#' @param - gddBaseTemp: The base temp to use for the any of the GDD equations. The default value of 10 will
#'                       be used if none is specified
#' @param - gdddMinBoundary: The minimum boundary to use in the selected GDD equation.
#'                           The behavior of this value is different depending on the equation you're using
#'                           The default value of 10 will be used if none is specified
#' @param - gddMaxBoundary: The max boundary to use in the selected GDD equation. The
#'                          behavior of this value is different depending on the equation you're using.
#'                          The default value of 30 will be used if none is specified
#'
#' @return data.table of requested data for dates requested
#'
#' @import httr
#' @import data.table
#' @import lubridate
#' @import jsonlite
#'
#' @examples
#' GetAgronomicNormsLatLon('39.8282', '-98.5795', '07-01', '07-10', '2008', '2015','2010,2011','','standard','10','10','30')

#' @export


GetAgronomicNormsLatLon <- function(latitude, longitude, monthDayStart, monthDayEnd = '',
                            yearStart, yearEnd,excludeYears = '',
                            accumulationStartDate = '',gddMethod = 'standard',gddBaseTemp = '10',
                            gddMinBoundary = '10', gddMaxBoundary = '30') {

  #############################################################
  #Checking Input Parameters
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

  monthDayStartTest <- strsplit(monthDayStart,'-')
  for (z in 1:length(monthDayStartTest[[1]])) {
    if (nchar(monthDayStartTest[[1]][z]) != 2) {
      warning('The paramter monthDayStart is not properly formatted.  Please correct. \n')
      return()
    }
  }
  if ((as.integer(monthDayStartTest[[1]][1]) >= 1 & as.integer(monthDayStartTest[[1]][1]) <= 12) == FALSE) {
    warning('The month paramater in monthDayStart is not a valid value.  Please correct. \n')
    return()
  }
  if (monthDayStartTest[[1]][1] %in% c('4','6','9','11')) {
    if ((as.integer(monthDayStartTest[[1]][2]) >= 1 & as.integer(monthDayStartTest[[1]][2]) <= 30) == FALSE) {
      warning('The day paramater in monthDayStart is not a valid value.  Please correct. \n')
      return()
    }
  } else if (monthDayStartTest[[1]][1] %in% c('2')) {
    if ((as.integer(monthDayStartTest[[1]][2]) >= 1 & as.integer(monthDayStartTest[[1]][2]) <= 28) == FALSE) {
      warning('The day paramater in monthDayStart is not a valid value.  Please correct. \n')
      return()
    }
  } else {
    if ((as.integer(monthDayStartTest[[1]][2]) >= 1 & as.integer(monthDayStartTest[[1]][2]) <= 31) == FALSE) {
      warning('The day paramater in monthDayStart is not a valid value.  Please correct. \n')
      return()
    }
  }

  if (monthDayEnd != '') {
    monthDayEndTest <- strsplit(monthDayEnd,'-')
    for (z in 1:length(monthDayEndTest[[1]])) {
      if (nchar(monthDayEndTest[[1]][z]) != 2) {
        warning('The paramter monthDayEnd is not properly formatted.  Please correct. \n')
        return()
      }
    }
    if ((as.integer(monthDayEndTest[[1]][1]) >= 1 & as.integer(monthDayEndTest[[1]][1]) <= 12) == FALSE) {
      warning('The month paramater in monthDayEnd is not a valid value.  Please correct. \n')
      return()
    }
    if (monthDayEndTest[[1]][1] %in% c('4','6','9','11')) {
      if ((as.integer(monthDayEndTest[[1]][2]) >= 1 & as.integer(monthDayEndTest[[1]][2]) <= 30) == FALSE) {
        warning('The day paramater in monthDayEnd is not a valid value.  Please correct. \n')
        return()
      }
    } else if (monthDayEndTest[[1]][1] %in% c('2')) {
      if ((as.integer(monthDayEndTest[[1]][2]) >= 1 & as.integer(monthDayEndTest[[1]][2]) <= 28) == FALSE) {
        warning('The day paramater in monthDayEnd is not a valid value.  Please correct. \n')
        return()
      }
    } else {
      if ((as.integer(monthDayEndTest[[1]][2]) >= 1 & as.integer(monthDayEndTest[[1]][2]) <= 31) == FALSE) {
        warning('The day paramater in monthDayEnd is not a valid value.  Please correct. \n')
        return()
      }
    }
  }

  if (yearStart != '') {
    if (as.integer(yearStart) < 1994 | as.integer(yearStart) > year(Sys.Date())) {
      warning('The yearStart paramater must be between 1994 and the current year.  Please correct. \n')
      return()
    }

    if (monthDayStart != '') {
      if (ymd(paste0(yearStart,'-',monthDayStart)) > ymd(Sys.Date())) {
        warning('The combination of yearStart and monthDayStart implies data from the future must be retrieved.  Please correct. \n')
        return()
      }
    }
  }

  if (yearEnd != '') {
    if (as.integer(yearEnd) < 1994 | as.integer(yearEnd) > year(Sys.Date())) {
      warning('The yearEnd paramater must be between 1994 and the current year.  Please correct. \n')
      return()
    }
    if (monthDayEnd != '') {
      if (ymd(paste0(yearEnd,'-',monthDayEnd)) > ymd(Sys.Date())) {
        warning('The combination of yearEnd and monthDayEnd implies data from the future must be retrieved.  Please correct. \n')
        return()
      }
    }
  }


  yearsToRequest <- seq(as.integer(yearStart),as.integer(yearEnd))

  if (excludeYears != '') {
    excludeYearsTest <- strsplit(excludeYears,',')
    for (z in 1:length(excludeYearsTest[[1]])) {
      if (as.integer(excludeYearsTest[[1]][z]) < 1994 | as.integer(excludeYearsTest[[1]][z]) > year(Sys.Date())) {
        warning('One of the years included in the excludeYears parameter is not in the \n
                 proper range (1994-CurrentYear).  Please correct. \n')
        return()
      }
      yearsToRequest <- yearsToRequest[yearsToRequest != as.integer(excludeYearsTest[[1]][z])]
    }
  }

  if (length(yearsToRequest) <= 3) {
    warning('At least three unique years must be used in this query. Please correct. \n')
    return()
  }

  if ((((yearStart != '') & (yearEnd == '')) | ((yearStart == '') & (yearEnd != ''))) == TRUE) {
    warning('Both the starting and ending years myst be specified explicitly if using years. \n')
    return()
  }

  if ((accumulationStartDate != '') == TRUE) {
    accumulationStartDateTest <- strsplit(accumulationStartDate,'-')
    for (z in 1:length(accumulationStartDateTest [[1]])) {
      if (nchar(accumulationStartDateTest[[1]][z]) != 2) {
        warning('The paramter accumulationStartDate is not properly formatted.  Please correct. \n')
        return()
      }
    }
    if ((as.integer(accumulationStartDateTest [[1]][1]) >= 1 & as.integer(accumulationStartDateTest [[1]][1]) <= 12) == FALSE) {
      warning('The month paramater in accumulationStartDate is not a valid value.  Please correct. \n')
      return()
    }
    if (accumulationStartDateTest [[1]][1] %in% c('4','6','9','11')) {
      if ((as.integer(accumulationStartDateTest [[1]][2]) >= 1 & as.integer(accumulationStartDateTest[[1]][2]) <= 30) == FALSE) {
        warning('The day paramater in accumulationStartDate is not a valid value.  Please correct. \n')
        return()
      }
    } else if (accumulationStartDateTest [[1]][1] %in% c('2')) {
      if ((as.integer(accumulationStartDateTest [[1]][2]) >= 1 & as.integer(accumulationStartDateTest [[1]][2]) <= 28) == FALSE) {
        warning('The day paramater in accumulationStartDate is not a valid value.  Please correct. \n')
        return()
      }
    } else {
      if ((as.integer(accumulationStartDateTest [[1]][2]) >= 1 & as.integer(accumulationStartDateTest[[1]][2]) <= 31) == FALSE) {
        warning('The day paramater in accumulationStartDate is not a valid value.  Please correct. \n')
        return()
      }
    }
    if (accumulationStartDateTest[[1]][1] == monthDayStartTest[[1]][1]) {
      if (accumulationStartDateTest[[1]][2] > monthDayStartTest[[1]][2]) {
        warning('The accumulationStartDate paramater must come before the startDate parameter.  Please correct. \n')
        return()
      }
    }
  }

  if ((gddMethod %in% c('standard','modifiedstandard','min-temp-cap','min-temp-constant')) == FALSE) {
    warning('Valid values for the GDD method used to calculate growing degree days are \n
            \'standard\', \'modifiedstandard\', \'min-temp-cap\', \'min-temp-constant\'.\n
            Please change gddMethod to one of these values. \n')
    return()
  }

  if (suppressWarnings(is.na(as.integer(gddBaseTemp))) == TRUE) {
    warning('The gddBaseTemp parameter is not a valid value.  Please correct. \n')
    return()
  }

  if (suppressWarnings(is.na(as.integer(gddMinBoundary))) == TRUE) {
    warning('The gddMinBoundary parameter is not a valid value.  Please correct. \n')
    return()
  }

  if (suppressWarnings(is.na(as.integer(gddMaxBoundary))) == TRUE) {
    warning('The gddMaxBoundary parameter is not a valid value.  Please correct. \n')
    return()
  }

  ##############################################################################
  dataList <- list()

  # Create query

  urlAddress <- "https://api.awhere.com/v2/agronomics"

  strBeg <- paste0('/locations')
  strCoord <- paste0('/',latitude,',',longitude)
  strType <- paste0('/agronomicnorms')

  if (monthDayEnd != '') {
    strMonthsDays <- paste0('/',monthDayStart,',',monthDayEnd)
  } else {
    strMonthsDays <- paste0('/',monthDayStart,',',monthDayStart)
  }

  if (excludeYears != '') {
    strExcludeYears <- paste0('&excludeYears=',excludeYears)
  } else {
    strExcludeYears <- ''
  }

  gddMethodString      <- paste0('?gddMethod=',gddMethod)
  gddBaseTempString    <- paste0('&gddBaseTemp=',gddBaseTemp)
  gddMinBoundaryString <- paste0('&gddMinBoundary=',gddMinBoundary)
  gddMaxBoundaryString <- paste0('&gddMinBoundary=',gddMaxBoundary)

  if (yearStart != '' & yearEnd != '') {
    strYearsType <- paste0('/years')
    strYears <- paste0('/',yearStart,',',yearEnd)
    address <- paste0(urlAddress, strBeg, strCoord, strType, strMonthsDays, strYearsType,
                      strYears,gddMethodString,gddBaseTempString,gddMinBoundaryString,gddMaxBoundaryString,excludeYears)
  } else {
    address <- paste0(urlAddress, strBeg, strCoord, strType, strMonthsDays,gddMethodString,
                      gddBaseTempString,gddMinBoundaryString,gddMaxBoundaryString,excludeYears)
  }
  doWeatherGet <- TRUE
  while (doWeatherGet == TRUE) {
    requestString <- 'request <- GET(address,
  	                                    add_headers(Authorization =
  	                                    paste0(\"Bearer \", awhereEnv75247$token)))'
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

  data <- as.data.table(x[[3]])

  varNames <- colnames(data)
  #This removes the non-data info returned with the JSON object
  data[,grep('_links',varNames) := NULL, with = FALSE]
  data[,grep('.units',varNames) := NULL, with = FALSE]

#   varNames <- colnames(data)
#
#   for (x in 1:length(varNames)) {
#
#     if (varNames[x] == 'day'){
#       varNames[x] <- 'monthDay'
#     } else if (varNames[x] == 'location.latitude'){
#       varNames[x] <- 'latitude'
#     } else if (varNames[x] == 'location.longitude'){
#       varNames[x] <- 'longitude'
#     } else if (varNames[x] == 'meanTemp.average'){
#       varNames[x] <- 'avgMeanTemperature'
#     } else if (varNames[x] == 'meanTemp.stdDev'){
#       varNames[x] <- 'avgMeanTemperatureStdDev'
#     } else if (varNames[x] == 'minTemp.average'){
#       varNames[x] <- 'avgMinTemperature'
#     } else if (varNames[x] == 'minTemp.stdDev'){
#       varNames[x] <- 'avgMinTemperatureStdDev'
#     } else if (varNames[x] == 'maxTemp.average'){
#       varNames[x] <- 'avgMaxTemperature'
#     } else if (varNames[x] == 'maxTemp.stdDev'){
#       varNames[x] <- 'avgMaxTemperatureStdDev'
#     } else if (varNames[x] == 'precipitation.average'){
#       varNames[x] <- 'avgPrecipitation'
#     } else if (varNames[x] == 'precipitation.stdDev'){
#       varNames[x] <- 'avgPrecipitationStdDev'
#     } else if (varNames[x] == 'solar.average'){
#       varNames[x] <- 'avgSolarEnergy'
#     } else if (varNames[x] == 'solar.stdDev'){
#       varNames[x] <- 'avgSolarEnergyStdDev'
#     } else if (varNames[x] == 'minHumidity.average'){
#       varNames[x] <- 'avgMinHumidity'
#     } else if (varNames[x] == 'minHumidity.stdDev'){
#       varNames[x] <- 'avgMinHumidityStdDev'
#     } else if (varNames[x] == 'maxHumidity.average'){
#       varNames[x] <- 'avgMaxHumidity'
#     } else if (varNames[x] == 'maxHumidity.stdDev'){
#       varNames[x] <- 'avgMaxHumidityStdDev'
#     } else if (varNames[x] == 'dailyMaxWind.average'){
#       varNames[x] <- 'avgDailyMaxWind'
#     } else if (varNames[x] == 'dailyMaxWind.stdDev'){
#       varNames[x] <- 'avgDailyMaxWindStdDev'
#     } else if (varNames[x] == 'averageWind.average'){
#       varNames[x] <- 'avgWind'
#     } else if (varNames[x] == 'averageWind.stdDev'){
#       varNames[x] <- 'avgWindStdDev'
#     }
#

  setnames(data,varNames)
  setkey(data,day)

  return(data)
}
