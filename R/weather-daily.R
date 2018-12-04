#' @title daily_observed_fields
#'
#' @description
#' \code{daily_observed_fields} pulls historical weather data from aWhere's API based on field id
#'
#' @details
#' This function returns weather data on Min/Max Temperature, Precipitation,
#' Min/Max Humidity, Solar Radiation, and Maximum Wind Speed,
#' Morning Max Windspeed, and Average Windspeed for the field id specified.
#' Default units are returned by the API.
#'
#' The Weather APIs provide access to aWhere's agriculture-specific Weather Terrain system,
#' and allows retrieval and integration of data across all different time ranges, long term normals,
#' daily observed, current weather, and forecasts. These APIs are designed for efficiency,
#' allowing you to customize the responses to return just the attributes you need.
#'
#' Understanding the recent and long-term daily weather is critical for making in-season decisions.
#' This API opens the weather attributes that matter most to agriculture.
#'
#' Note about dates: The system does not adjust for any difference in dates between the location of the user
#'           and where data is being requested from.  It is the responsibility of the user to ensure a valid
#'           date range is specified given any differences in timezone.  These differences can have implications
#'           for whether a given date should be requested from the daily_observed functions or the forecast functions
#'
#' @references http://developer.awhere.com/api/reference/weather/observations
#'
#' @param - field_id: the field_id associated with the location for which you want to pull data.
#' Field IDs are created using the create_field function.(string)
#' @param - day_start: character string of the first day for which you want to retrieve data, in the form: YYYY-MM-DD.
#' @param - day_end: character string of the last day for which you want to retrieve data, in form: YYYY-MM-DD
#' @param - propertiesToInclude: character vector of properties to retrieve from API.  Valid values are temperatures, precipitation, solar, relativeHumidity, wind (optional)
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
#'
#' @examples
#' \dontrun{daily_observed_fields(field_id = 'field_test'
#'                                ,day_start = '2018-04-28'
#'                                ,day_end = '2018-05-01')}

#' @export
daily_observed_fields <- function(field_id
                                  ,day_start
                                  ,day_end
                                  ,propertiesToInclude = ''
                                  ,keyToUse = awhereEnv75247$uid
                                  ,secretToUse = awhereEnv75247$secret
                                  ,tokenToUse = awhereEnv75247$token) {

  checkCredentials(keyToUse,secretToUse,tokenToUse)
  checkValidField(field_id,keyToUse,secretToUse,tokenToUse)
  checkValidStartEndDates(day_start,day_end)
  checkPropertiesEndpoint('weather',propertiesToInclude)


  ## Create Request
  #Calculate number of loops needed if requesting more than 120 days
  numObsReturned <- 120

  if (day_start != '' & day_end != '') {
    numOfDays <- as.numeric(difftime(lubridate::ymd(day_end), lubridate::ymd(day_start), units = 'days'))
    allDates <- seq(as.Date(lubridate::ymd(day_start)),as.Date(lubridate::ymd(day_end)), by="days")

    loops <- ((length(allDates))) %/% numObsReturned
    remainder <- ((length(allDates))) %% numObsReturned

  } else if (day_start != '') {

    numOfDays <- 1
    allDates <- lubridate::ymd(day_start)
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

  # loop through, making requests in chunks of size numObsReturned

  for (i in 1:loops) {

    starting = numObsReturned*(i-1)+1
    ending = numObsReturned*i

    if(paste(allDates,sep = '',collapse ='') != '') {
      day_start_toUse <- allDates[starting]
      day_end_toUse <- allDates[ending]
      if(is.na(day_end_toUse)) {
        tempDates <- allDates[c(starting:length(allDates))]
        day_start_toUse <- tempDates[1]
        day_end_toUse <- tempDates[length(tempDates)]
      }
    }


    # Create query

    urlAddress <- "https://api.awhere.com/v2/weather"

    strBeg <- paste0('/fields')
    strCoord <- paste0('/',field_id)
    strType <- paste0('/observations')

    if(paste(allDates,sep = '',collapse ='') != '') {
      strDates <- paste0('/',day_start_toUse,',',day_end_toUse)

      returnedAmount <- as.integer(difftime(lubridate::ymd(day_end_toUse),lubridate::ymd(day_start_toUse),units = 'days')) + 1L
      if (returnedAmount > numObsReturned) {
        returnedAmount <- numObsReturned
      }
      limitString <- paste0('?limit=',returnedAmount)

    } else {
      strDates <- ''
      limitString <- paste0('?limit=',numObsReturned)
    }

    if (propertiesToInclude[1] != '') {
      propertiesString <- paste0('&properties=',paste0(propertiesToInclude,collapse = ','))
    } else {
      propertiesString <- ''
    }

    url <- paste0(urlAddress, strBeg, strCoord, strType, strDates, limitString,propertiesString)

    doWeatherGet <- TRUE

    while (doWeatherGet == TRUE) {
      postbody = ''
      request <- httr::GET(url, body = postbody, httr::content_type('application/json'),
                           httr::add_headers(Authorization =paste0("Bearer ", tokenToUse)))

      a <- suppressMessages(httr::content(request, as = "text"))

      doWeatherGet <- check_JSON(a,request)
    }

    #The JSONLITE Serializer properly handles the JSON conversion
    x <- jsonlite::fromJSON(a, flatten = TRUE)

    data <- data.table::as.data.table(x[[1]])

    dataList[[i]] <- data
  }

  allWeath <- rbindlist(dataList)

  varNames <- colnames(allWeath)

  #This removes the non-data info returned with the JSON object
  allWeath[,grep('_links',varNames) := NULL]
  allWeath[,grep('.units',varNames) := NULL]

  currentNames <- data.table::copy(colnames(allWeath))
  allWeath[,field_id  := field_id]
  data.table::setcolorder(allWeath,c('field_id',currentNames))

  checkDataReturn_daily(allWeath,day_start,day_end)

  return(as.data.frame(allWeath))
}


#' @title daily_observed_latlng
#'
#' @description
#' \code{daily_observed_latlng} pulls historical weather data from aWhere's API based on latitude & longitude
#'
#' @details
#' This function returns weather data on Min/Max Temperature, Precipitation,
#' Min/Max Humidity, Solar Radiation, and Maximum Wind Speed,
#' Morning Max Windspeed, and Average Windspeed for the location specified by latitude and longitude.
#' Default units are returned by the API. Latitude and longitude must be in decimal degrees.
#'
#' The Weather APIs provide access to aWhere's agriculture-specific Weather Terrain system,
#' and allows retrieval and integration of data across all different time ranges, long term normals,
#' daily observed, current weather, and forecasts. These APIs are designed for efficiency,
#' allowing you to customize the responses to return just the attributes you need.
#'
#' Understanding the recent and long-term daily weather is critical for making in-season decisions.
#' This API opens the weather attributes that matter most to agriculture.
#'
#' Note about dates: The system does not adjust for any difference in dates between the location of the user
#'           and where data is being requested from.  It is the responsibility of the user to ensure a valid
#'           date range is specified given any differences in timezone.  These differences can have implications
#'           for whether a given date should be requested from the daily_observed functions or the forecast functions
#'
#' @references http://developer.awhere.com/api/reference/weather/observations/geolocation
#'
#' @param - latitude: the latitude of the requested location (double)
#' @param - longitude: the longitude of the requested locations (double)
#' @param - day_start: character string of the first day for which you want to retrieve data, in the form: YYYY-MM-DD
#' @param - day_end: character string of the last day for which you want to retrieve data, in the form: YYYY-MM-DD
#' @param - propertiesToInclude: character vector of properties to retrieve from API.  Valid values are temperatures, precipitation, solar, relativeHumidity, wind (optional)
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
#'
#' @examples
#' \dontrun{daily_observed_latlng(latitude = 39.8282
#'                                ,longitude = -98.5795
#'                                ,day_start = '2018-04-28'
#'                                ,day_end = '2018-05-01')}

#' @export


daily_observed_latlng <- function(latitude
                                  ,longitude
                                  ,day_start
                                  ,day_end
                                  ,propertiesToInclude = ''
                                  ,keyToUse = awhereEnv75247$uid
                                  ,secretToUse = awhereEnv75247$secret
                                  ,tokenToUse = awhereEnv75247$token) {

  checkCredentials(keyToUse,secretToUse,tokenToUse)
  checkValidLatLong(latitude,longitude)
  checkValidStartEndDates(day_start,day_end)
  checkPropertiesEndpoint('weather',propertiesToInclude)

  ## Create Request
  #Calculate number of loops needed if requesting more than 120 days
  numObsReturned <- 120

  if (day_end != '') {
    numOfDays <- as.numeric(difftime(lubridate::ymd(day_end), lubridate::ymd(day_start), units = 'days'))
    allDates <- seq(as.Date(lubridate::ymd(day_start)),as.Date(lubridate::ymd(day_end)), by="days")

    loops <- ((length(allDates))) %/% numObsReturned
    remainder <- ((length(allDates))) %% numObsReturned

  } else {

    numOfDays <- 1
    allDates <- lubridate::ymd(day_start)
    loops <- 1
    remainder <- 0
  }

  if(remainder > 0) {
    loops <- loops + 1
  }
  i <- 1

  dataList <- list()

  # loop through, making requests in chunks of size numObsReturned

  for (i in 1:loops) {

    starting = numObsReturned*(i-1)+1
    ending = numObsReturned*i
    day_start_toUse <- allDates[starting]
    day_end_toUse <- allDates[ending]
    if(is.na(day_end_toUse)) {
      tempDates <- allDates[c(starting:length(allDates))]
      day_start_toUse <- tempDates[1]
      day_end_toUse   <- tempDates[length(tempDates)]
    }


    # Create query

    urlAddress <- "https://api.awhere.com/v2/weather"

    strBeg <- paste0('/locations')
    strCoord <- paste0('/',latitude,',',longitude)
    strType <- paste0('/observations')
    strDates <- paste0('/',day_start_toUse,',',day_end_toUse)


    returnedAmount <- as.integer(difftime(lubridate::ymd(day_end_toUse),lubridate::ymd(day_start_toUse),units = 'days')) + 1L
    if (returnedAmount > numObsReturned) {
      returnedAmount <- numObsReturned
    }
    limitString <- paste0('?limit=',returnedAmount)

    if (propertiesToInclude[1] != '') {
      propertiesString <- paste0('&properties=',paste0(propertiesToInclude,collapse = ','))
    } else {
      propertiesString <- ''
    }

    url <- paste0(urlAddress, strBeg, strCoord, strType, strDates, limitString,propertiesString)

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

    data <- data.table::as.data.table(x[[1]])

    dataList[[i]] <- data

  }
  allWeath <- rbindlist(dataList)

  varNames <- colnames(allWeath)

  #This removes the non-data info returned with the JSON object
  allWeath[,grep('_links',varNames) := NULL]
  allWeath[,grep('.units',varNames) := NULL]

  currentNames <- data.table::copy(colnames(allWeath))
  allWeath[,latitude  := latitude]
  allWeath[,longitude := longitude]
  data.table::setcolorder(allWeath,c('latitude','longitude',currentNames))

  checkDataReturn_daily(allWeath,day_start,day_end)

  return(as.data.frame(allWeath))
}

#' @title daily_observed_area
#'
#' @description
#' \code{daily_observed_area} pulls historical weather data from aWhere's API for a provided polygon or extent
#'
#' @details
#' This function returns weather data on Min/Max Temperature, Precipitation,
#' Min/Max Humidity, Solar Radiation, and Maximum Wind Speed,
#' Morning Max Windspeed, and Average Windspeed for the polygon passed to the function.
#' Default units are returned by the API. The polygon should be either a SpatialPolygons object or
#' a well-known text character string or an extent.
#'
#' The Weather APIs provide access to aWhere's agriculture-specific Weather Terrain system,
#' and allows retrieval and integration of data across all different time ranges, long term normals,
#' daily observed, current weather, and forecasts. These APIs are designed for efficiency,
#' allowing you to customize the responses to return just the attributes you need.
#'
#' Understanding the recent and long-term daily weather is critical for making in-season decisions.
#' This API opens the weather attributes that matter most to agriculture.
#'
#' Note about dates: The system does not adjust for any difference in dates between the location of the user
#'           and where data is being requested from.  It is the responsibility of the user to ensure a valid
#'           date range is specified given any differences in timezone.  These differences can have implications
#'           for whether a given date should be requested from the daily_observed functions or the forecast functions.
#'           Furthermore, because this function can take as input locations that may be in different timezones, it is
#'           the responsibility of the user to either ensure that the date range specified is valid for all relevant
#'           locations or to break the query into pieces.
#'
#' @references http://developer.awhere.com/api/reference/weather/observations/geolocation
#'
#' @param - polygon: either a SpatialPolygons object, well-known text string, or extent from raster package
#' @param - day_start: character string of the first day for which you want to retrieve data, in the form: YYYY-MM-DD
#' @param - day_end: character string of the last day for which you want to retrieve data, in the form: YYYY-MM-DD
#' @param - propertiesToInclude: character vector of properties to retrieve from API.  Valid values are temperatures, precipitation, solar, relativeHumidity, wind (optional)
#' @param - numcores: number of cores to use in parallel loop. To check number of available cores: parallel::detectCores()
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
#' @import foreach
#' @import doParallel
#' @import rgeos
#'
#' @return data.frame of requested data for dates requested
#'
#'
#' @examples
#' \dontrun{daily_observed_area(polygon = raster::getData('GADM', country = "Gambia", level = 0, download = T),
#'                                ,day_start = '2018-04-28'
#'                                ,day_end = '2018-05-01'
#'                                ,numcores = 2)}

#' @export


daily_observed_area <- function(polygon
                                ,day_start
                                ,day_end
                                ,propertiesToInclude = ''
                                ,numcores = 2
                                ,bypassNumCallCheck = FALSE
                                ,keyToUse = awhereEnv75247$uid
                                ,secretToUse = awhereEnv75247$secret
                                ,tokenToUse = awhereEnv75247$token) {

  checkCredentials(keyToUse,secretToUse,tokenToUse)
  checkValidStartEndDates(day_start,day_end)

  cat(paste0('Creating aWhere Raster Grid within Polygon\n'))
  grid <- create_awhere_grid(polygon)

  verify_api_calls(grid,bypassNumCallCheck)

  cat(paste0('Requesting data using parallal API calls\n'))
  doParallel::registerDoParallel(cores=numcores)

  observed <- foreach::foreach(j=c(1:nrow(grid)), .packages = c("aWhereAPI")) %dopar% {

    t <- daily_observed_latlng(latitude = grid$lat[j]
                               ,longitude = grid$lon[j]
                               ,day_start = day_start
                               ,day_end = day_end
                               ,propertiesToInclude = propertiesToInclude
                               ,keyToUse = keyToUse
                               ,secretToUse = secretToUse
                               ,tokenToUse = tokenToUse)

    currentNames <- colnames(t)

    t$gridy <- grid$gridy[j]
    t$gridx <- grid$gridx[j]

    data.table::setcolorder(t, c(currentNames[c(1:2)], "gridy", "gridx", currentNames[c(3:length(currentNames))]))

    return(t)

  }

  observed <- data.table::rbindlist(observed)
  return(as.data.frame(observed))
}
