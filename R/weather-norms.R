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

    if (grepl('API Access Expired',a)) {
      if(exists("awhereEnv75247")) {
        if(tokenToUse == awhereEnv75247$token) {
          get_token(keyToUse,secretToUse)
          tokenToUse <- awhereEnv75247$token
        } else {
          stop("The token you passed in has expired. Please request a new one and retry your function call with the new token.")
        }
      } else {
        stop("The token you passed in has expired. Please request a new one and retry your function call with the new token.")
      }
    } else {
      aWhereAPI:::checkStatusCode(request)
      doWeatherGet <- FALSE
    }
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

    if (grepl('API Access Expired',a)) {
      if(exists("awhereEnv75247")) {
        if(tokenToUse == awhereEnv75247$token) {
          get_token(keyToUse,secretToUse)
          tokenToUse <- awhereEnv75247$token
        } else {
          stop("The token you passed in has expired. Please request a new one and retry your function call with the new token.")
        }
      } else {
        stop("The token you passed in has expired. Please request a new one and retry your function call with the new token.")
      }
    } else {
      aWhereAPI:::checkStatusCode(request)
      doWeatherGet <- FALSE
    }
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
#' \code{weather_norms_area} pulls long term norm weather data from aWhere's API based on latitude & longitude
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
#' @param - propertiesToInclude: character vector of properties to retrieve from API.  Valid values are meanTemp, maxTemp, minTemp, precipitation, solar, maxHumidity, minHumidity, dailyMaxWind (optional)
#' @param - exclude_year: Year or years which you'd like to exclude from
#'                        your range of years on which to calculate norms. To exclude
#'                        multiple years, provide a vector of years. You must include
#'                       at least three years of data with which to calculate the norms. (numeric, optional)
#' @param - includeFeb29thData: Whether to keep data from Feb 29th on leap years.  Because weather/agronomics
#'                              summary statistics are calculated via the calendar date and 3 years are required
#'                              to generate a value, data from this date is more likely to be NA.  ALlows user
#'                              to drop this data to avoid later problems (defaults to TRUE)
#' @param - numcores: number of cores to use in parallel loop. To check number of available cores: parallel::detectCores()
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
#' @import rgeos
#'
#' @return data.frame of requested data for dates requested
#'
#' @examples
#' \dontrun{weather_norms_area(polygon = raster::getData('GADM', country = "Gambia", level = 0, download = F)
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
                               ,keyToUse = awhereEnv75247$uid
                               ,secretToUse = awhereEnv75247$secret
                               ,tokenToUse = awhereEnv75247$token) {

  #Checking Input Parameters
  checkCredentials(keyToUse,secretToUse,tokenToUse)
  checkNormsStartEndDates(monthday_start,monthday_end)
  checkNormsYearsToRequest(year_start,year_end,monthday_start,monthday_end,exclude_years)
  checkPropertiesEndpoint('weather_norms',propertiesToInclude)

  ##############################################################################


  ## If polygon is WKT, convert to SpatialPolygons class
  if(class(polygon) == "character") {
    tryCatch({polygon <- rgeos::readWKT(polygon)}, error = function(e) {
      stop(e)
    })
  }

  cat(paste0('Creating aWhere Raster Grid within Polygon\n'))
  ## Create grid of lat/lon points within given polygon
  ## aWhere grid is spaced at .08333 decimal degrees resolution,
  ## so .08 should guarantee a grid point in each aWhere grid cell
  grid <- suppressWarnings(sp::makegrid(raster::buffer(polygon, .5), cellsize = .08))
  grid <- sp::SpatialPoints(grid, proj4string = sp::CRS(sp::proj4string(polygon)))
  grid <- grid[polygon,]

  grid <- as.data.frame(grid@coords)

  colnames(grid) <- c("lon", "lat")

  ## Calculate GridX and GridY for each calculated grid point
  grid$gridx <- getGridX(grid$lon)
  grid$gridy <- getGridY(grid$lat)

  ## Keep a only unique GridX/GridY pairings
  grid <- unique(grid[,c("gridx", "gridy")])

  ## Calculate lat and lon for each GridX/GridY
  grid$lon <- getLongitude(grid$gridx)
  grid$lat <- getLatitude(grid$gridy)

  cat(paste0('This query will require ',nrow(grid),' API Calls \n'))
  makeAPICalls <- readline("\n Do you wish to proceed? Type yes to begin API calls")

  if (tolower(makeAPICalls) != 'yes') {
    stop('User Input indicated they did not want to proceed with making API Calls')
  }


  cat(paste0('Requesting data using parallal API calls\n'))
  doParallel::registerDoParallel(cores=numcores)

  norms <- foreach::foreach(j=c(1:nrow(grid)), .packages = c("aWhereAPI")) %dopar% {


    return(weather_norms_latlng(latitude = grid$lat[j]
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
                               ,tokenToUse = tokenToUse))


  }

  norms <- data.table::rbindlist(norms)

  return(as.data.frame(norms))
}
