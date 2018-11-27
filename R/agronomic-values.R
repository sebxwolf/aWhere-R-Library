#' @title agronomic_values_fields
#'
#' @description
#' \code{agronomic_values_fields} pulls agronomic data from aWhere's API based on field id
#'
#' @details
#' This function returns agronomic data on growing degree days (GDDs), potential evapotranspiration (PET), Precipitation over
#' potential evapotranspiration (P/PET), accumulated GDDs, accumulated precipitation, accumulated PET, and
#' accumulated P/PET.  Default units are returned by the API.
#'
#' Agronomic Values are calculated numbers that can be used to show the agronomic status of a field or crop.
#' These figures can be used, for example, to track and predict plant growth or identify water stress.
#' Accumulated values allow growers to easily identify how the weather has been over the season.
#' Both sets of data are commonly used on small and large farms alike.  This is a very flexible API
#' that supports a wide variety of configurations to get exactly the data you want as efficiently as
#' possible. It's also designed to work with the Fields and Plantings system to reduce the amount of input.
#' While a planting is not required to use this API, creating a Planting for your Fields will allow you
#' to get the most out of the aWhere platform.
#'
#' Note about dates: The system does not adjust for any difference in dates between the location of the user
#'           and where data is being requested from.  It is the responsibility of the user to ensure a valid
#'           date range is specified given any differences in timezone.  These differences can have implications
#'           for whether a given date should be requested from the daily_observed functions or the forecast functions
#'
#' @references http://developer.awhere.com/api/reference/agronomics/values
#'
#' @param - field_id: the field_id associated with the location for which you want to pull data.
#' Field IDs are created using the create_field function. (string)
#' @param - day_start: character string of the first day for which you want to retrieve data, in the form: YYYY-MM-DD
#' @param - day_end: character string of the last day for which you want to retrieve data, in the form: YYYY-MM-DD
#' @param - propertiesToInclude: character vector of properties to retrieve from API.  Valid values are accumulations, gdd, pet, ppet, accumulatedGdd, accumulatedPrecipitation, accumulatedPet, accumulatedPpet (optional)
#' @param - accumulation_start_date: Allows the user to start counting accumulations from
#'                                 before the specified start date (or before the
#'                                 planting date if using the most recent planting).
#'                                 Use this parameter to specify the date from which
#'                                 you wish to start counting, in the form: YYYY-MM-DD.
#'                                 The daily values object
#'                                 will still only return the days between the start
#'                                 and end date. This date must come before the start date. (optional)
#' @param - gdd_method: There are variety of equations available for calculating growing degree-days.
#'                     Valid entries are: 'standard', 'modifiedstandard', 'min-temp-cap', 'min-temp-constant'
#'                     See the API documentation for a description of each method.  The standard
#'                     method will be used if none is specified. (character - optional)
#' @param - gdd_base_temp: The base temp to use for the any of the GDD equations. The default value of 10 will
#'                       be used if none is specified. (optional)
#' @param - gdd_min_boundary: The minimum boundary to use in the selected GDD equation.
#'                           The behavior of this value is different depending on the equation you're using
#'                           The default value of 10 will be used if none is specified. (optional)
#' @param - gdd_max_boundary: The max boundary to use in the selected GDD equation. The
#'                          behavior of this value is different depending on the equation you're using.
#'                          The default value of 30 will be used if none is specified. (optional)
#' @param - keyToUse: aWhere API uid to use.  For advanced use only.  Most users will not need to use this parameter (optional)
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
#' \dontrun{agronomic_values_fields(field_id = "field_test"
#'                                  ,day_start = "2018-05-01"
#'                                  ,day_end = "2018-05-31"
#'                                  ,gdd_method = "modifiedstandard"
#'                                  ,gdd_base_temp = 10
#'                                  ,gdd_min_boundary = 10
#'                                  ,gdd_max_boundary = 30)}
#' @export

agronomic_values_fields <- function(field_id
                                    ,day_start
                                    ,day_end
                                    ,propertiesToInclude = ''
                                    ,accumulation_start_date = ''
                                    ,gdd_method = 'standard'
                                    ,gdd_base_temp = 10
                                    ,gdd_min_boundary = 10
                                    ,gdd_max_boundary = 30
                                    ,keyToUse = awhereEnv75247$uid
                                    ,secretToUse = awhereEnv75247$secret
                                    ,tokenToUse = awhereEnv75247$token) {

  checkCredentials(keyToUse,secretToUse,tokenToUse)
  checkValidField(field_id,keyToUse,secretToUse,tokenToUse)
  checkValidStartEndDatesAgronomics(day_start,day_end)
  checkGDDParams(gdd_method,gdd_base_temp,gdd_min_boundary,gdd_max_boundary)
  checkAccumulationStartDate(accumulation_start_date, day_start)
  checkPropertiesEndpoint('agronomics',propertiesToInclude)

  # Create query
  urlAddress <- "https://api.awhere.com/v2/agronomics"

  strBeg <- paste0('/fields')
  strCoord <- paste0('/',field_id)
  strType <- paste0('/agronomicvalues')

  if (as.character(day_start) != '' & as.character(day_end) != '') {
    strDates <- paste0('/',day_start,',',day_end)
  } else if (day_end != '') {
    strDates <- paste0('/',day_start,',',day_start)
  } else {
    strDates <- ''
  }

  if (accumulation_start_date != '') {
    strAccumulation <- paste0('&accumulationStartDate=',accumulation_start_date)
  } else {
    strAccumulation <- ''
  }

  gdd_methodString       <- paste0('?gddMethod=',gdd_method)
  gdd_base_tempString    <- paste0('&gddBaseTemp=',gdd_base_temp)
  gdd_min_boundaryString <- paste0('&gddMinBoundary=',gdd_min_boundary)
  gdd_max_boundaryString <- paste0('&gddMaxBoundary=',gdd_max_boundary)

  if (propertiesToInclude[1] != '') {
    propertiesString <- paste0('&properties=',paste0(propertiesToInclude,collapse = ','))
  } else {
    propertiesString <- ''
  }

  url <- paste0(urlAddress, strBeg, strCoord, strType, strDates,
                    gdd_methodString,gdd_base_tempString,gdd_min_boundaryString,
                    gdd_max_boundaryString,strAccumulation,propertiesString)

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

  if (propertiesToInclude[1] != '' & any(grepl('accumulated',propertiesToInclude,fixed = TRUE)) == FALSE) {
    data <- as.data.table(x[[1]])
  } else if (propertiesToInclude[1] != '' & any(grepl('accumulated',propertiesToInclude,fixed = TRUE)) == TRUE) {
    data <- as.data.table(x[[2]])
  } else {
    data <- as.data.table(x[[3]])
  }

  varNames <- colnames(data)
  suppressWarnings(data[,grep('_links',varNames) := NULL])
  suppressWarnings(data[,grep('.units',varNames) := NULL])

  currentNames <- data.table::copy(colnames(data))
  data[,field_id  := field_id]
  data.table::setcolorder(data,c('field_id',currentNames))

  checkDataReturn_daily(data,day_start,day_end)

  return(as.data.frame(data))
  }


#' @title agronomic_values_latlng.
#'
#' @description
#' \code{agronomic_values_latlng} pulls agronomic data from aWhere's API based on latitude & longitude
#'
#' @details
#' This function returns agronomic data on GDDs, potential evapotranspiration (PET), Precipitation over
#' potential evapotranspiration (P/PET), accumulated GDDs, accumulated precipitation, accumulated PET, and
#' accumulated P/PET.  Default units are returned by the API.
#'
#' Agronomic Values are calculated numbers that can be used to show the agronomic status of a field or crop.
#' These figures can be used, for example, to track and predict plant growth or identify water stress.
#' Accumulated values allow growers to easily identify how the weather has been over the season.
#' Both sets of data are commonly used on small and large farms alike.  This is a very flexible API
#' that supports a wide variety of configurations to get exactly the data you want as efficiently as
#' possible.
#'
#' Note about dates: The system does not adjust for any difference in dates between the location of the user
#'           and where data is being requested from.  It is the responsibility of the user to ensure a valid
#'           date range is specified given any differences in timezone.  These differences can have implications
#'           for whether a given date should be requested from the daily_observed functions or the forecast functions
#'
#' @references http://developer.awhere.com/api/reference/agronomics/values
#'
#' @param - latitude: the latitude of the requested location (double)
#' @param - longitude: the longitude of the requested locations (double)
#' @param - day_start: character string of the first day for which you want to retrieve data, in the form: YYYY-MM-DD
#' @param - day_end: character string of the last day for which you want to retrieve data, in the form: YYYY-MM-DD
#' @param - propertiesToInclude: character vector of properties to retrieve from API.  Valid values are accumulations, gdd, pet, ppet, accumulatedGdd, accumulatedPrecipitation, accumulatedPet, accumulatedPpet (optional)
#' @param - accumulation_start_date: Allows the user to start counting accumulations from
#'                                 before the specified start date (or before the
#'                                 planting date if using the most recent planting).
#'                                 Use this parameter to specify the date from which
#'                                 you wish to start counting, in the form: YYYY-MM-DD.
#'                                 The daily values object
#'                                 will still only return the days between the start
#'                                 and end date. This date must come before the start date. (optional)
#' @param - gdd_method: There are variety of equations available for calculating growing degree-days.
#'                     Valid entries are: 'standard', 'modifiedstandard', 'min-temp-cap', 'min-temp-constant'
#'                     See the API documentation for a description of each method.  The standard
#'                     method will be used if none is specified. (character - optional)
#' @param - gdd_base_temp: The base temp to use for the any of the GDD equations. The default value of 10 will
#'                       be used if none is specified. (optional)
#' @param - gdd_min_boundary: The minimum boundary to use in the selected GDD equation.
#'                           The behavior of this value is different depending on the equation you're using
#'                           The default value of 10 will be used if none is specified. (optional)
#' @param - gdd_max_boundary: The max boundary to use in the selected GDD equation. The
#'                          behavior of this value is different depending on the equation you're using.
#'                          The default value of 30 will be used if none is specified. (optional)
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
#' \dontrun{agronomic_values_latlng(latitude = 39.8282
#'                                  ,longitude = -98.5795
#'                                  ,day_start = '2018-05-01'
#'                                  ,day_end = '2018-05-31')}
#' @export


agronomic_values_latlng <- function(latitude
                                    ,longitude
                                    ,day_start
                                    ,day_end
                                    ,propertiesToInclude = ''
                                    ,accumulation_start_date = ''
                                    ,gdd_method = 'standard'
                                    ,gdd_base_temp = 10
                                    ,gdd_min_boundary = 10
                                    ,gdd_max_boundary = 30
                                    ,keyToUse = awhereEnv75247$uid
                                    ,secretToUse = awhereEnv75247$secret
                                    ,tokenToUse = awhereEnv75247$token) {

  checkCredentials(keyToUse,secretToUse,tokenToUse)
  checkValidLatLong(latitude,longitude)
  checkValidStartEndDatesAgronomics(day_start,day_end)
  checkGDDParams(gdd_method,gdd_base_temp,gdd_min_boundary,gdd_max_boundary)
  checkAccumulationStartDate(accumulation_start_date)
  checkPropertiesEndpoint('agronomics',propertiesToInclude)

  # Create query

  urlAddress <- "https://api.awhere.com/v2/agronomics"

  strBeg <- paste0('/locations')
  strCoord <- paste0('/',latitude,',',longitude)
  strType <- paste0('/agronomicvalues')
  strDates <- paste0('/',day_start,',',day_end)

  gdd_methodString      <- paste0('?gddMethod=',gdd_method)
  gdd_base_tempString    <- paste0('&gddBaseTemp=',gdd_base_temp)
  gdd_min_boundaryString <- paste0('&gddMinBoundary=',gdd_min_boundary)
  gdd_max_boundaryString <- paste0('&gddMaxBoundary=',gdd_max_boundary)

  if (accumulation_start_date != '') {
    strAccumulation <- paste0('&accumulationStartDate=',accumulation_start_date)
  } else {
    strAccumulation <- ''
  }

  if (propertiesToInclude[1] != '') {
    propertiesString <- paste0('&properties=',paste0(propertiesToInclude,collapse = ','))
  } else {
    propertiesString <- ''
  }

  url <- paste0(urlAddress, strBeg, strCoord, strType, strDates,
                    gdd_methodString,gdd_base_tempString,gdd_min_boundaryString,
                    gdd_max_boundaryString,strAccumulation,propertiesString)

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

  if (propertiesToInclude[1] != '' & any(grepl('accumulated',propertiesToInclude,fixed = TRUE)) == FALSE) {
    data <- as.data.table(x[[1]])
  } else if (propertiesToInclude[1] != '' & any(grepl('accumulated',propertiesToInclude,fixed = TRUE)) == TRUE) {
    data <- as.data.table(x[[2]])
  } else {
    data <- as.data.table(x[[3]])
  }

  varNames <- colnames(data)
  suppressWarnings(data[,grep('_links',varNames) := NULL])
  suppressWarnings(data[,grep('.units',varNames) := NULL])

  currentNames <- data.table::copy(colnames(data))
  data[,latitude  := latitude]
  data[,longitude := longitude]
  data.table::setcolorder(data,c('latitude','longitude',currentNames))

  checkDataReturn_daily(data,day_start,day_end)

  return(as.data.frame(data))
}

#' @title agronomic_values_area
#'
#' @description
#' \code{agronomic_values_area} pulls agronomic data from aWhere's API based on spatial polygon or extent
#'
#' @details
#' This function returns agronomic data on GDDs, potential evapotranspiration (PET), Precipitation over
#' potential evapotranspiration (P/PET), accumulated GDDs, accumulated precipitation, accumulated PET, and
#' accumulated P/PET.  Default units are returned by the API.
#'
#' Agronomic Values are calculated numbers that can be used to show the agronomic status of a field or crop.
#' These figures can be used, for example, to track and predict plant growth or identify water stress.
#' Accumulated values allow growers to easily identify how the weather has been over the season.
#' Both sets of data are commonly used on small and large farms alike.  This is a very flexible API
#' that supports a wide variety of configurations to get exactly the data you want as efficiently as
#' possible.
#'
#' The polygon should be either a SpatialPolygons object or a well-known text character string or an extent.
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
#' @param - propertiesToInclude: character vector of properties to retrieve from API.  Valid values are accumulations, gdd, pet, ppet, accumulatedGdd, accumulatedPrecipitation, accumulatedPet, accumulatedPpet (optional)
#' @param - accumulation_start_date: Allows the user to start counting accumulations from
#'                                 before the specified start date (or before the
#'                                 planting date if using the most recent planting).
#'                                 Use this parameter to specify the date from which
#'                                 you wish to start counting, in the form: YYYY-MM-DD.
#'                                 The daily values object
#'                                 will still only return the days between the start
#'                                 and end date. This date must come before the start date. (optional)
#' @param - gdd_method: There are variety of equations available for calculating growing degree-days.
#'                     Valid entries are: 'standard', 'modifiedstandard', 'min-temp-cap', 'min-temp-constant'
#'                     See the API documentation for a description of each method.  The standard
#'                     method will be used if none is specified. (character - optional)
#' @param - gdd_base_temp: The base temp to use for the any of the GDD equations. The default value of 10 will
#'                       be used if none is specified. (optional)
#' @param - gdd_min_boundary: The minimum boundary to use in the selected GDD equation.
#'                           The behavior of this value is different depending on the equation you're using
#'                           The default value of 10 will be used if none is specified. (optional)
#' @param - gdd_max_boundary: The max boundary to use in the selected GDD equation. The
#'                          behavior of this value is different depending on the equation you're using.
#'                          The default value of 30 will be used if none is specified. (optional)#' @param - numcores: number of cores to use in parallel loop. To check number of available cores: parallel::detectCores()
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
#' \dontrun{agronomic_values_area(polygon = raster::getData('GADM', country = "Gambia", level = 0, download = T),
#'                                ,day_start = '2018-04-28'
#'                                ,day_end = '2018-05-01'
#'                                ,numcores = 2)}

#' @export


agronomic_values_area <- function(polygon
                                ,day_start
                                ,day_end
                                ,propertiesToInclude = ''
                                ,accumulation_start_date = ''
                                ,gdd_method = 'standard'
                                ,gdd_base_temp = 10
                                ,gdd_min_boundary = 10
                                ,gdd_max_boundary = 30
                                ,numcores = 2
                                ,bypassNumCallCheck = FALSE
                                ,keyToUse = awhereEnv75247$uid
                                ,secretToUse = awhereEnv75247$secret
                                ,tokenToUse = awhereEnv75247$token) {

  checkCredentials(keyToUse,secretToUse,tokenToUse)
  checkValidStartEndDatesAgronomics(day_start,day_end)
  checkGDDParams(gdd_method,gdd_base_temp,gdd_min_boundary,gdd_max_boundary)
  checkAccumulationStartDate(accumulation_start_date)
  checkPropertiesEndpoint('agronomics',propertiesToInclude)

  cat(paste0('Creating aWhere Raster Grid within Polygon\n'))
  grid <- create_awhere_grid(polygon)

  verify_api_calls(grid,bypassNumCallCheck)

  cat(paste0('Requesting data using parallal API calls\n'))
  doParallel::registerDoParallel(cores=numcores)

  observed <- foreach::foreach(j=c(1:nrow(grid)), .packages = c("aWhereAPI")) %dopar% {

    t <- agronomic_values_latlng(latitude = grid$lat[j]
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


