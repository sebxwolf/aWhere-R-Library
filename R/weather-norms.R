#' @title weather_norms_fields
#'
#' @description
#' \code{weather_norms_fields} pulls long term norm weather data from aWhere's API based on field id
#'
#' @details
#' This function allows you to calculate the averages for weather attributes
#' across any range of years for which data are available.  The data pulled includes
#' meanTemp, maxTemp, minTemp, precipitation average, solar radiation average,
#' minHumidity, maxHumidity, maxWind and averageWind, along with the standard deviations
#' for these variables.  The data pulled is for the field id identified.
#'
#' The data returned in this function
#' allow you to compare this year or previous years to the long-term normals, calculated as
#' the average of those weather conditions on that day in that location over the years specified.
#'
#' @references http://developer.awhere.com/api/reference/weather/norms
#'
#' @param - field_id: the field_id associated with the location for which you want to pull data.
#'                    Field IDs are created using the create_field function. (string)
#' @param - monthday_start: character string of the first month and day for which you want to retrieve data,
#'                          in the form: MM-DD.  This is the start of your date range. e.g. '07-01' (July 1) (required)
#' @param - monthday_end: character string of the last month and day for which you want to retrieve data,
#'                          in the form: MM-DD.  This is the end of your date range. e.g. '07-01' (July 1) (required)
#' @param - year_start: character string of the starting year (inclusive) of the range of years for which
#'                     you're calculating norms, in the form YYYY. e.g., 2008 (required)
#' @param - year_end: character string of the last year (inclusive) of the range of years for which
#'                     you're calculating norms, in the form YYYY. e.g., 2015 (required)
#' @param - exclude_year: Year or years which you'd like to exclude from
#'                        your range of years on which to calculate norms. To exclude
#'                        multiple years, provide a vector of years. You must include
#'                       at least three years of data with which to calculate the norms. (numeric, optional)
#' @param - keyToUse: aWhere API key to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#' @param - secretToUse: aWhere API secret to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#' @param - tokenToUse: aWhere API token to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#'
#' @import httr
#' @import data.table
#' @import lubridate
#' @import jsonlite
#'
#' @return dataframe of requested data for dates requested
#'
#' @examples
#' \dontrun{weather_norms_fields("field_test", monthday_start = "06-01", monthday_end = "09-01",
#'                                year_start = 2006, year_end = 2015)}
#' @export
#'
weather_norms_fields <- function(field_id
                                 ,monthday_start
                                 ,monthday_end
                                 ,year_start
                                 ,year_end
                                 ,exclude_years = NULL
                                 ,keyToUse = awhereEnv75247$uid
                                 ,secretToUse = awhereEnv75247$secret
                                 ,tokenToUse = awhereEnv75247$token) {

  #Checking Input Parameters
  checkCredentials(keyToUse,secretToUse,tokenToUse)
  checkValidField(field_id,keyToUse,secretToUse,tokenToUse)
  checkNormsStartEndDates(monthday_start,monthday_end)
  checkNormsYearsToRequest(year_start,year_end,monthday_start,monthday_end,exclude_years)


  ##############################################################################
  dataList <- list()

  # Create query

  urlAddress <- "https://api.awhere.com/v2/weather"

  strBeg <- paste0('/fields')
  strCoord <- paste0('/',field_id)
  strType <- paste0('/norms')

  if (monthday_start != '' & monthday_end != '') {
    strMonthsDays <- paste0('/',monthday_start,',',monthday_end)
  } else if (monthday_end != '') {
    strMonthsDays <- paste0('/',monthday_start,',',monthday_start)
  } else {
    strMonthsDays <- ''
  }

  if (length(exclude_years) != 0) {
    strexclude_years <- paste0('?excludeYears=',toString(exclude_years))
  } else {
    strexclude_years <- ''
  }

  strYearsType <- paste0('/years')
  strYears <- paste0('/',year_start,',',year_end)
  url <- paste0(urlAddress, strBeg, strCoord, strType, strMonthsDays, strYearsType,strYears,strexclude_years)

  doWeatherGet <- TRUE
  while (doWeatherGet == TRUE) {
    postbody = ''
    request <- httr::GET(url, body = postbody, httr::content_type('application/json'),
                         httr::add_headers(Authorization =paste0("Bearer ", tokenToUse)))
    # Make request

    a <- suppressMessages(httr::content(request, as = "text"))

    if (grepl('API Access Expired',a)) {
      get_token(keyToUse,secretToUse)
    } else {
      checkStatusCode(request)
      doWeatherGet <- FALSE
    }
  }

  #The JSONLITE Serializer properly handles the JSON conversion
  x <- jsonlite::fromJSON(a,flatten = TRUE)

  data <- data.table::as.data.table(x$norms)

  varNames <- copy(colnames(data))
  #This removes the non-data info returned with the JSON object
  data[,grep('_links',varNames) := NULL]
  data[,grep('.units',varNames) := NULL]

  checkDataReturn_norms(data,monthday_start,monthday_end,year_start,year_end,exclude_years)

  return(as.data.frame(data))
}

#' @title weather_norms_latlng
#'
#' @description
#' \code{weather_norms_latlng} pulls long term norm weather data from aWhere's API based on latitude & longitude
#'
#' @details
#' This function allows you to calculate the averages for weather attributes
#' across any range of years for which data are available.  The data pulled includes
#' meanTemp, maxTemp, minTemp, precipitation average, solar radiation average,
#' minHumidity, maxHumidity, maxWind and averageWind, along with the standard deviations
#' for these variables.  The data pulled is for the latitude & longitude identified.
#'
#' The data returned in this function
#' allow you to compare this year or previous years to the long-term normals, calculated as
#' the average of those weather conditions on that day in that location over the years specified.
#'
#' @references http://developer.awhere.com/api/reference/weather/norms
#'
#' @param - latitude: the latitude of the requested location (double, required)
#' @param - longitude: the longitude of the requested locations (double, required)
#' @param - monthday_start: character string of the first month and day for which you want to retrieve data,
#'                          in the form: MM-DD.  This is the start of your date range. e.g. '07-01' (July 1) (required)
#' @param - monthday_end: character string of the last month and day for which you want to retrieve data,
#'                          in the form: MM-DD.  This is the end of your date range. e.g. '07-01' (July 1) (required)
#' @param - year_start: character string of the starting year (inclusive) of the range of years for which
#'                     you're calculating norms, in the form YYYY. e.g., 2008 (required)
#' @param - year_end: character string of the last year (inclusive) of the range of years for which
#'                     you're calculating norms, in the form YYYY. e.g., 2015 (required)
#' @param - exclude_year: Year or years which you'd like to exclude from
#'                        your range of years on which to calculate norms. To exclude
#'                        multiple years, provide a vector of years. You must include
#'                       at least three years of data with which to calculate the norms. (numeric, optional)
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
#' \dontrun{weather_norms_latlng(39.8282, -98.5795, '07-01', '07-10', 2008, 2015, "2010")}
#' @export


weather_norms_latlng <- function(latitude
                                 ,longitude
                                 ,monthday_start
                                 ,monthday_end
                                 ,year_start
                                 ,year_end
                                 ,exclude_years = NULL
                                 ,keyToUse = awhereEnv75247$uid
                                 ,secretToUse = awhereEnv75247$secret
                                 ,tokenToUse = awhereEnv75247$token) {

  #Checking Input Parameters
  checkCredentials(keyToUse,secretToUse,tokenToUse)
  checkValidLatLong(latitude,longitude)
  checkNormsStartEndDates(monthday_start,monthday_end)
  checkNormsYearsToRequest(year_start,year_end,monthday_start,monthday_end,exclude_years)


  ##############################################################################
  dataList <- list()

  # Create query

  urlAddress <- "https://api.awhere.com/v2/weather"

  strBeg <- paste0('/locations')
  strCoord <- paste0('/',latitude,',',longitude)
  strType <- paste0('/norms')

  if (monthday_end != '') {
    strMonthsDays <- paste0('/',monthday_start,',',monthday_end)
  } else {
    strMonthsDays <- paste0('/',monthday_start,',',monthday_start)
  }

  if (length(exclude_years) != 0) {
    strexclude_years <- paste0('?excludeYears=',toString(exclude_years))
  } else {
    strexclude_years <- ''
  }

  if (year_start != '' & year_end != '') {
    strYearsType <- paste0('/years')
    strYears <- paste0('/',year_start,',',year_end)
    url <- paste0(urlAddress, strBeg, strCoord, strType, strMonthsDays, strYearsType,strYears,strexclude_years)
  } else {
    url <- paste0(urlAddress, strBeg, strCoord, strType, strMonthsDays, strexclude_years)
  }

  doWeatherGet <- TRUE
  while (doWeatherGet == TRUE) {
    postbody = ''
    request <- httr::GET(url, body = postbody, httr::content_type('application/json'),
                         httr::add_headers(Authorization =paste0("Bearer ", tokenToUse)))

    a <- suppressMessages(httr::content(request, as = "text"))

    if (grepl('API Access Expired',a)) {
      get_token(keyToUse,secretToUse)
    } else {
      checkStatusCode(request)
      doWeatherGet <- FALSE
    }
  }

  #The JSONLITE Serializer properly handles the JSON conversion
  x <- jsonlite::fromJSON(a,flatten = TRUE)

  data <- data.table::as.data.table(x[[1]])

  varNames <- copy(colnames(data))
  #This removes the non-data info returned with the JSON object
  data[,grep('_links',varNames) := NULL]
  data[,grep('.units',varNames) := NULL]

  checkDataReturn_norms(data,monthday_start,monthday_end,year_start,year_end,exclude_years)

  return(as.data.frame(data))
}
