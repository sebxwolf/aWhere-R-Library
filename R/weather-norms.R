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
#' Note about dates: The system does not adjust for any difference in dates between the location of the user
#'           and where data is being requested from.  It is the responsibility of the user to ensure a valid
#'           date range is specified given any differences in timezone.  These differences can have implications
#'           for whether a given date should be requested from the daily_observed functions or the forecast functions
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
#' @param - propertiesToInclude: character vector of properties to retrieve from API.  Valid values are meanTemp, maxTemp, minTemp, precipitation, solar, maxHumidity, minHumidity, dailyMaxWind (optional)
#' @param - exclude_year: Year or years which you'd like to exclude from
#'                        your range of years on which to calculate norms. To exclude
#'                        multiple years, provide a vector of years. You must include
#'                       at least three years of data with which to calculate the norms. (numeric, optional)
#' @param - includeFeb29thData: Whether to keep data from Feb 29th on leap years.  Because weather/agronomics
#'                              summary statistics are calculated via the calendar date and 3 years are required
#'                              to generate a value, data from this date is more likely to be NA.  ALlows user
#'                              to drop this data to avoid later problems (defaults to TRUE)
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
#' \dontrun{weather_norms_fields(field_id = "field_test"
#'                               ,monthday_start = "06-01"
#'                               ,monthday_end = "09-01"
#'                               ,year_start = 2006
#'                               ,year_end = 2015)}
#' @export
#'
weather_norms_fields <- function(field_id
                                 ,monthday_start
                                 ,monthday_end
                                 ,year_start
                                 ,year_end
                                 ,propertiesToInclude = ''
                                 ,exclude_years = NULL
                                 ,includeFeb29thData = TRUE
                                 ,keyToUse = awhereEnv75247$uid
                                 ,secretToUse = awhereEnv75247$secret
                                 ,tokenToUse = awhereEnv75247$token) {

  #Checking Input Parameters
  checkCredentials(keyToUse,secretToUse,tokenToUse)
  checkValidField(field_id,keyToUse,secretToUse,tokenToUse)
  checkNormsStartEndDates(monthday_start,monthday_end)
  checkNormsYearsToRequest(year_start,year_end,monthday_start,monthday_end,exclude_years)
  checkPropertiesEndpoint('weather_norms',propertiesToInclude)

  ##############################################################################

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

  if (propertiesToInclude[1] != '') {
    if (strexclude_years == '') {
      propertiesString <- paste0('?properties=',paste0(propertiesToInclude,collapse = ','))
    } else {
      propertiesString <- paste0('&properties=',paste0(propertiesToInclude,collapse = ','))
    }
  } else {
    propertiesString <- ''
  }


  strYearsType <- paste0('/years')
  strYears <- paste0('/',year_start,',',year_end)
  url <- paste0(urlAddress, strBeg, strCoord, strType, strMonthsDays, strYearsType,strYears,strexclude_years,propertiesString)

  doWeatherGet <- TRUE
  while (doWeatherGet == TRUE) {
    postbody = ''
    request <- httr::GET(url, body = postbody, httr::content_type('application/json'),
                         httr::add_headers(Authorization =paste0("Bearer ", tokenToUse)))
    # Make request

    a <- suppressMessages(httr::content(request, as = "text"))

    doWeatherGet <- check_JSON(a,request)
  }

  #The JSONLITE Serializer properly handles the JSON conversion
  x <- jsonlite::fromJSON(a,flatten = TRUE)

  data <- data.table::as.data.table(x$norms)

  #Get rid of leap yearData
  if (includeFeb29thData == FALSE) {
    data <- data[day != '02-29',]
  }

  varNames <- colnames(data)
  #This removes the non-data info returned with the JSON object
  data[,grep('_links',varNames) := NULL]
  data[,grep('.units',varNames) := NULL]

  currentNames <- data.table::copy(colnames(data))
  data[,field_id  := field_id]
  data.table::setcolorder(data,c('field_id',currentNames))

  checkDataReturn_norms(data,monthday_start,monthday_end,year_start,year_end,exclude_years,includeFeb29thData)

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
#' Note about dates: The system does not adjust for any difference in dates between the location of the user
#'           and where data is being requested from.  It is the responsibility of the user to ensure a valid
#'           date range is specified given any differences in timezone.  These differences can have implications
#'           for whether a given date should be requested from the daily_observed functions or the forecast functions
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
#' @param - propertiesToInclude: character vector of properties to retrieve from API.  Valid values are meanTemp, maxTemp, minTemp, precipitation, solar, maxHumidity, minHumidity, dailyMaxWind (optional)
#' @param - exclude_year: Year or years which you'd like to exclude from
#'                        your range of years on which to calculate norms. To exclude
#'                        multiple years, provide a vector of years. You must include
#'                       at least three years of data with which to calculate the norms. (numeric, optional)
#' @param - includeFeb29thData: Whether to keep data from Feb 29th on leap years.  Because weather/agronomics
#'                              summary statistics are calculated via the calendar date and 3 years are required
#'                              to generate a value, data from this date is more likely to be NA.  ALlows user
#'                              to drop this data to avoid later problems (defaults to TRUE)
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
#' \dontrun{weather_norms_latlng(latitude = 39.8282
#'                               ,longitude = -98.5795
#'                               ,monthday_start = '02-01'
#'                               ,monthday_end = '03-10'
#'                               ,year_start = 2008
#'                               ,year_end = 2015
#'                               ,exclude_years =  c(2010,2011))}
#' @export


weather_norms_latlng <- function(latitude
                                 ,longitude
                                 ,monthday_start
                                 ,monthday_end
                                 ,year_start
                                 ,year_end
                                 ,propertiesToInclude = ''
                                 ,exclude_years = NULL
                                 ,includeFeb29thData = TRUE
                                 ,keyToUse = awhereEnv75247$uid
                                 ,secretToUse = awhereEnv75247$secret
                                 ,tokenToUse = awhereEnv75247$token) {

  #Checking Input Parameters
  aWhereAPI:::checkCredentials(keyToUse,secretToUse,tokenToUse)
  aWhereAPI:::checkValidLatLong(latitude,longitude)
  aWhereAPI:::checkNormsStartEndDates(monthday_start,monthday_end)
  aWhereAPI:::checkNormsYearsToRequest(year_start,year_end,monthday_start,monthday_end,exclude_years)
  aWhereAPI:::checkPropertiesEndpoint('weather_norms',propertiesToInclude)

  ##############################################################################

  # Create query

  urlAddress <- "https://api.awhere.com/v2/weather"

  strBeg <- paste0('/locations')
  strCoord <- paste0('/',latitude,',',longitude)
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

  if (propertiesToInclude[1] != '') {
    if (strexclude_years == '') {
      propertiesString <- paste0('?properties=',paste0(propertiesToInclude,collapse = ','))
    } else {
      propertiesString <- paste0('&properties=',paste0(propertiesToInclude,collapse = ','))
    }
  } else {
    propertiesString <- ''
  }

  strYearsType <- paste0('/years')
  strYears <- paste0('/',year_start,',',year_end)
  url <- paste0(urlAddress, strBeg, strCoord, strType, strMonthsDays, strYearsType,strYears,strexclude_years,propertiesString)

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

  data <- data.table::as.data.table(x[[1]])

  #Get rid of leap yearData
  if (includeFeb29thData == FALSE) {
    data <- data[day != '02-29',]
  }

  varNames <- colnames(data)
  #This removes the non-data info returned with the JSON object
  data[,grep('_links',varNames) := NULL]
  data[,grep('.units',varNames) := NULL]

  currentNames <- data.table::copy(colnames(data))
  data[,latitude  := latitude]
  data[,longitude := longitude]
  data.table::setcolorder(data,c('latitude','longitude',currentNames))

  aWhereAPI:::checkDataReturn_norms(data,monthday_start,monthday_end,year_start,year_end,exclude_years,includeFeb29thData)

  return(as.data.frame(data))
}


#' @title weather_norms_area
#'
#' @description
#' \code{weather_norms_area} pulls long term norm weather data from aWhere's API based on spatial polygon or extent
#'
#' @details
#' This function allows you to calculate the averages for weather attributes
#' across any range of years for which data are available.  The data pulled includes
#' meanTemp, maxTemp, minTemp, precipitation average, solar radiation average,
#' minHumidity, maxHumidity, maxWind and averageWind, along with the standard deviations
#' for these variables.  The polygon should be either a SpatialPolygons object or
#' a well-known text character string or an extent.
#'
#' The data returned in this function
#' allow you to compare this year or previous years to the long-term normals, calculated as
#' the average of those weather conditions on that day in that location over the years specified.
#'
#' Note about dates: The system does not adjust for any difference in dates between the location of the user
#'           and where data is being requested from.  It is the responsibility of the user to ensure a valid
#'           date range is specified given any differences in timezone.  These differences can have implications
#'           for whether a given date should be requested from the daily_observed functions or the forecast functions.
#'           Furthermore, because this function can take as input locations that may be in different timezones, it is
#'           the responsibility of the user to either ensure that the date range specified is valid for all relevant
#'           locations or to break the query into pieces.
#'
#' @references http://developer.awhere.com/api/reference/weather/norms
#'
#' @param - polygon: either a SpatialPolygons object, well-known text string, or extent from raster package
#' @param - monthday_start: character string of the first month and day for which you want to retrieve data,
#'                          in the form: MM-DD.  This is the start of your date range. e.g. '07-01' (July 1) (required)
#' @param - monthday_end: character string of the last month and day for which you want to retrieve data,
#'                          in the form: MM-DD.  This is the end of your date range. e.g. '07-01' (July 1) (required)
#' @param - year_start: character string of the starting year (inclusive) of the range of years for which
#'                     you're calculating norms, in the form YYYY. e.g., 2008 (required)
#' @param - year_end: character string of the last year (inclusive) of the range of years for which
#'                     you're calculating norms, in the form YYYY. e.g., 2015 (required)
#' @param - propertiesToInclude: character vector of properties to retrieve from API.  Valid values are meanTemp, maxTemp, minTemp, precipitation, solar, maxHumidity, minHumidity, dailyMaxWind (optional)
#' @param - exclude_year: Year or years which you'd like to exclude from
#'                        your range of years on which to calculate norms. To exclude
#'                        multiple years, provide a vector of years. You must include
#'                       at least three years of data with which to calculate the norms. (numeric, optional)
#' @param - includeFeb29thData: Whether to keep data from Feb 29th on leap years.  Because weather/agronomics
#'                              summary statistics are calculated via the calendar date and 3 years are required
#'                              to generate a value, data from this date is more likely to be NA.  ALlows user
#'                              to drop this data to avoid later problems (defaults to TRUE)
#' @param - numcores: number of cores to use in parallel loop. To check number of available cores: parallel::detectCores().
#'                    If you receive an error regarding the speed you are making calls, reduce this number
#' @param - bypassNumCallCheck: set to TRUE to avoid prompting the user to confirm that they want to begin making API calls
#' @param - keyToUse: aWhere API key to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#' @param - secretToUse: aWhere API secret to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#' @param - tokenToUse: aWhere API token to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#'
#' @import httr
#' @import data.table
#' @import lubridate
#' @import jsonlite
#' @import raster
#' @import foreach
#' @import doParallel
#' @import rgeos
#'
#' @return data.frame of requested data for dates requested
#'
#' @examples
#' \dontrun{weather_norms_area(polygon = raster::getData('GADM', country = "Gambia", level = 0, download = T)
#'                               ,monthday_start = '02-01'
#'                               ,monthday_end = '03-10'
#'                               ,year_start = 2008
#'                               ,year_end = 2015
#'                               ,exclude_years =  c(2010,2011)
#'                               ,numcores = 2)}
#' @export


weather_norms_area <- function(polygon
                               ,monthday_start
                               ,monthday_end
                               ,year_start
                               ,year_end
                               ,propertiesToInclude = ''
                               ,exclude_years = NULL
                               ,includeFeb29thData = TRUE
                               ,numcores = 2
                               ,bypassNumCallCheck = FALSE
                               ,keyToUse = awhereEnv75247$uid
                               ,secretToUse = awhereEnv75247$secret
                               ,tokenToUse = awhereEnv75247$token) {

  #Checking Input Parameters
  checkCredentials(keyToUse,secretToUse,tokenToUse)
  checkNormsStartEndDates(monthday_start,monthday_end)
  checkNormsYearsToRequest(year_start,year_end,monthday_start,monthday_end,exclude_years)
  checkPropertiesEndpoint('weather_norms',propertiesToInclude)

  ##############################################################################

  cat(paste0('Creating aWhere Raster Grid within Polygon\n'))
  grid <- create_awhere_grid(polygon)

  verify_api_calls(grid,bypassNumCallCheck)

  cat(paste0('Requesting data using parallal API calls\n'))
  doParallel::registerDoParallel(cores=numcores)

  norms <- foreach::foreach(j=c(1:nrow(grid)), .packages = c("aWhereAPI")) %dopar% {


    t <- weather_norms_latlng(latitude = grid$lat[j]
                              ,longitude = grid$lon[j]
                              ,monthday_start = monthday_start
                              ,monthday_end = monthday_end
                              ,year_start = year_start
                              ,year_end = year_end
                              ,propertiesToInclude = propertiesToInclude
                              ,exclude_years =  exclude_years
                              ,includeFeb29thData = includeFeb29thData
                              ,keyToUse = keyToUse
                              ,secretToUse = secretToUse
                              ,tokenToUse = tokenToUse)

    currentNames <- colnames(t)

    t$gridy <- grid$gridy[j]
    t$gridx <- grid$gridx[j]

    data.table::setcolorder(t, c(currentNames[c(1:2)], "gridy", "gridx", currentNames[c(3:length(currentNames))]))

    return(t)


  }

  norms <- data.table::rbindlist(norms)

  return(as.data.frame(norms))
}
