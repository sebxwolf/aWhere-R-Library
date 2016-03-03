#' @title GetCurrentConditionsLatLon.
#'
#' @description
#' \code{GetCurrentConditionsLatLon} calls Current Weather Conditions Endpoint of API using Lat/Lon Constuct
#'
#' @details
#' The Weather APIs provide access to aWhere's agriculture-specific Weather Terrain™ system,
#' and allows retrieval and integration of data across all different time ranges—long term normals,
#' daily observed, current weather, and forecasts. These APIs are designed for efficiency,
#' allowing you to customize the responses to return just the attributes you need.
#'
#' Forecasts are useful for predicting today's weather at the start of the day, but conditions
#' can change quickly. At that point, having a more recent reading of the area's weather is key
#' for making decisions in the middle of the day. The Current Weather Conditions, or Nowcast,
#' API provides the current conditions. This API uses the data from the nearest weather station
#' that is capable of hourly updates.  Uses the Lat/Lon construct for requestion data.
#' Uses default units returned by the API
#'
#' @references http://developer.awhere.com/api/reference/weather/current
#'
#' @param - latitude: the latitude of the requested location
#' @param - longitude: the longitude of the requested locations
#' @param - sources: Which source to use for pulling the current conditions.  Valid values are
#'                    'metar', 'mesonet', 'metar-mesonet', 'pws', 'all'.  Default value is 'all'
#' @return data.table of requested data for dates requested
#'
#' @import httr
#' @import data.table
#' @import lubridate
#' @import jsonlite
#'
#' @examples
#' GetCurrentConditionsLatLon('39.8282', '-98.5795','all')

#' @export


GetCurrentConditionsLatLon <- function(latitude,longitude,sources = 'all') {

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

  if ((sources %in% c('metar','mesonet','metar-mesonet','pws','all')) == FALSE) {
    warning('The specified source is not valid. Please correct. \n')
    return()
  }

  # Create query

  urlAddress <- "https://api.awhere.com/v2/weather"

  strBeg <- paste0('/locations')
  strCoord <- paste0('/',latitude,',',longitude)
  strType <- paste0('/currentconditions')
  strSources <- paste0('?sources=',sources)

  address <- paste0(urlAddress, strBeg, strCoord, strType, strSources)

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
    } else if (grepl('Javascript runtime error',a)) {
      doWeatherGet <- TRUE
    } else {
      doWeatherGet <- FALSE
    }
  }

  data <- as.data.table(data.frame(as.list(unlist(x)),stringsAsFactors = FALSE))

  varNames <- colnames(data)

  #This removes the non-data info returned with the JSON object
  data[,grep('_links',varNames) := NULL, with = FALSE]
  data[,grep('.units',varNames) := NULL, with = FALSE]

  return(data)
}
