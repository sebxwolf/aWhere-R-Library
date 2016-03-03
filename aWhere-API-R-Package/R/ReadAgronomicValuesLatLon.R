#' @title GetAgronomicValuesLatLon.
#'
#' @description
#' \code{GetAgronomicValuesLatLon} calls Agronomic Values and Accumulations by Geolocation Endpoint of API using Lat/Lon Constuct
#'
#' @details
#' Agronomic Values are calculated numbers that can be used to show the agronomic status of a field or crop.
#' These figures can be used, for example, to track and predict plant growth or identify water stress.
#' Accumulated values allow growers to easily identify how the weather has been over the season.
#' Both sets of data are commonly used on small and large farms alike.  This is a very flexible API
#' that supports a wide variety of configurations to get exactly the data you want as efficiently as
#' possible. This function used the Lat/Lon construct to request data.  This function returns GDDs, Pet,
#' P/Pet, accumulated GDD, accumulated Precipitation, accumulated Pet, accumulated P/Pet.  Returns data
#' in API default units (metric)
#'
#' @references http://developer.awhere.com/api/reference/agronomics/values/geolocation
#'
#' @param - latitude: the latitude of the requested location
#' @param - longitude: the longitude of the requested locations
#' @param - dayStart: character string of start date in form: YYYY-MM-DD
#'                    Defaults to system date -1 if left blank
#' @param - dayEnd: character string of end date in form: YYYY-MM-DD
#'                  If Not included will return data only for start date
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
#' @return data.table of requested data for dates requested
#'
#' @import httr
#' @import data.table
#' @import lubridate
#' @import jsonlite
#'
#' @examples
#' GetAgronomicValuesLatLon('39.8282', '-98.5795','2015-07-01','2015-07-31','','standard','10','10','30')

#' @export


GetAgronomicValuesLatLon <- function(latitude, longitude,
                                       dayStart = ymd(Sys.Date()) - days(1), dayEnd = '',
                                       accumulationStartDate = '',gddMethod = 'standard',gddBaseTemp = '10',
                                       gddMinBoundary = '10', gddMaxBoundary = '30') {

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
    } else if (ymd(dayEnd) > ymd(Sys.Date()) - days(1)) {
      warning('By default, this function can only be used to access data up until yesterday. \n
              Use the GetForecast function to request data from today onward.\n')
      return()
    }
  }

  if ((accumulationStartDate != '') == TRUE) {
    if (suppressWarnings(is.na(ymd(accumulationStartDate))) == TRUE) {
      warning('The Accumulation Start Date is Not Properly Formatted.  Please change to proper format. \n')
      return()
    }
  }

  if (ymd(dayStart) <= ymd(Sys.Date())-months(30)) {
    warning('By default, the aWhere APIs only allow daily data from the previous 30 months. \n
             Use the Norms API for long-term averages or speak to your account manager for longer access.\n')
    return()
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

    urlAddress <- "https://api.awhere.com/v2/agronomics"

    strBeg <- paste0('/locations')
    strCoord <- paste0('/',latitude,',',longitude)
    strType <- paste0('/agronomicvalues')
    strDates <- paste0('/',dayStart,',',dayEnd)

    gddMethodString      <- paste0('&gddMethod=',gddMethod)
    gddBaseTempString    <- paste0('&gddBaseTemp=',gddBaseTemp)
    gddMinBoundaryString <- paste0('&gddMinBoundary=',gddMinBoundary)
    gddMaxBoundaryString <- paste0('&gddMinBoundary=',gddMaxBoundary)

    accumulationStartDateString = paste0('&accumulationStartDate=',accumulationStartDate)

    returnedAmount <- as.integer(difftime(ymd(dayEnd),ymd(dayStart),units = 'days')) + 1L
    if (returnedAmount > numObsReturned) {
      returnedAmount <- numObsReturned
    }
    limitString <- paste0('?limit=',returnedAmount)

    if (accumulationStartDate == '') {
      address <- paste0(urlAddress, strBeg, strCoord, strType, strDates, limitString,
                        gddMethodString,gddBaseTempString,gddMinBoundaryString,gddMaxBoundaryString)
    } else {
      address <- paste0(urlAddress, strBeg, strCoord, strType, strDates, limitString,
                        gddMethodString,gddBaseTempString,gddMinBoundaryString,gddMaxBoundaryString,accumulationStartDateString)
    }

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

    data <- as.data.table(x[[3]])

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
