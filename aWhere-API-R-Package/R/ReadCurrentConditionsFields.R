#' @title GetCurrentConditionsFields.
#'
#' @description
#' \code{GetCurrentConditionsFields} calls Current Weather Conditions Endpoint of API using Fields Name Constuct
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
#' that is capable of hourly updates.  Uses the Fields Name construct for requestion data.
#' Uses default units returned by the API
#'
#' @references http://developer.awhere.com/api/reference/weather/current
#'
#' @param - fieldId: the fieldId having previously been created with the createField Function
#' @param - sources: Which source to use for pulling the current conditions.  Valid values are
#'                    'metar', 'mesonet', 'metar-mesonet', 'pws', 'all'.  Default value is 'all'
#' @return data.table of requested data for dates requested
#'
#' @import httr
#' @import data.table
#' @import jsonlite
#'
#' @examples
#' GetCurrentConditionsFields('field123','all')

#' @export


GetCurrentConditionsFields <- function(fieldId,sources = 'all') {

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

  currentFields <- GetFields(fieldId)
  if ((fieldId %in% currentFields$fieldId) == FALSE) {
    warning('The Provided field name is not a field currently associated with your account. \n
            Please create the field before proceeding. \n')
    return()
  }

  if ((sources %in% c('metar','mesonet','metar-mesonet','pws','all')) == FALSE) {
    warning('The specified source is not valid. Please correct. \n')
    return()
  }

  # Create query

  urlAddress <- "https://api.awhere.com/v2/weather"

  strBeg <- paste0('/fields')
  strCoord <- paste0('/',fieldId)
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

    a <- suppressMessages(content(request, as = "text"))

    #The JSONLITE Serializer properly handles the JSON conversion

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

  return(as.data.frame(data))
}
