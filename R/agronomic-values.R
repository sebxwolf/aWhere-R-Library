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

    if (grepl('API Access Expired',a)) {
      get_token(keyToUse,secretToUse)
    } else {
      checkStatusCode(request)
      doWeatherGet <- FALSE
    }
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
#' possible. It's also designed to work with the Fields and Plantings system to reduce the amount of input.
#' While a planting is not required to use this API, creating a Planting for your Fields will allow you
#' to get the most out of the aWhere platform.
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

    if (grepl('API Access Expired',a)) {
      get_token(keyToUse,secretToUse)
    } else {
      checkStatusCode(request)
      doWeatherGet <- FALSE
    }
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

