#' @title agronomic_norms_fields
#'
#' @description
#' \code{agronomic_norms_fields} pulls agronomic norm data from aWhere's API based on field id
#'
#' @details
#' This function allows you to calculate the averages for agronomic attributes
#' across any range of years for which data are available.  The data pulled includes norms for
#' growing degree days (GDDs), potential evapotranspiration (PET), Precipitation over
#' potential evapotranspiration (P/PET), accumulated GDDs, accumulated precipitation,
#' accumulated PET, and accumulated P/PET, along with the standard deviations
#' for these variables.  The data pulled is for the field id identified.
#' Default units are returned by the API.
#'
#' The data returned in this function
#' allow you to compare this year or previous years to the long-term normals, calculated as
#' the average of those agronomic conditions on that day in that location over the years specified.
#'
#'
#'
#' @references http://developer.awhere.com/api/reference/agronomics/norms
#'
#' @param - field_id: the field_id associated with the location for which you want to pull data.
#' Field IDs are created using the create_field function. (string)
#' @param - monthday_start: character string of the first month and day for which you want to retrieve data,
#'                          in the form: MM-DD.  This is the start of your date range. e.g. '07-01' (July 1) (required)
#' @param - monthday_end: character string of the last month and day for which you want to retrieve data,
#'                          in the form: MM-DD.  This is the end of your date range. e.g. '07-01' (July 1) (required)
#' @param - year_start: character string of the starting year (inclusive) of the range of years for which
#'                     you're calculating norms, in the form YYYY. e.g., 2008 (required)
#' @param - year_end: character string of the last year (inclusive) of the range of years for which
#'                     you're calculating norms, in the form YYYY. e.g., 2015 (required)
#' @param - exclude_year: character string of a year or years which you'd like to exclude from
#'                        your range of years on which to calculate norms. To exclude
#'                        multiple years, provide a vector of years. You must include
#'                       at least three years of data with which to calculate the norms. (optional)
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
#'
#'
#' @import httr
#' @import data.table
#' @import lubridate
#' @import jsonlite
#'
#' @return dataframe of requested data for dates requested
#'
#' @examples
#' \dontrun{agronomic_norms_fields(field_id = 'field_test', month_day_start = '07-01', month_day_end = '07-10',
#'                                 year_start = 2008, year_end = 2016, exclude_years = "2010",
#'                                  accumulation_start_date = '', gdd_method = 'standard', gdd_base_temp = 10,
#'                                  gdd_min_boundary = 10, gdd_max_boundary = 30)}

#' @export


agronomic_norms_fields <- function(field_id, month_day_start, month_day_end,
                                   year_start, year_end, exclude_years = c(),
                                   accumulation_start_date = '', gdd_method = 'standard',
                                   gdd_base_temp = 10, gdd_min_boundary = 10, gdd_max_boundary = 30) {

  #############################################################
  #Checking Input Parameters
  checkCredentials()
  checkValidField(field_id)
  checkGDDParams(gdd_method,gdd_base_temp,gdd_min_boundary,gdd_max_boundary)
  checkNormsStartEndDates(month_day_start,month_day_end)
  checkNormsYearsToRequest(year_start,year_end,month_day_start,month_day_end,exclude_years)
  checkAccumulationStartDateNorms(accumulation_start_date,month_day_start)

  # Create query

  urlAddress <- "https://api.awhere.com/v2/agronomics"

  strBeg <- paste0('/fields')
  strCoord <- paste0('/',field_id)
  strType <- paste0('/agronomicnorms')

  if (month_day_start != '' & month_day_end != '') {
    strMonthsDays <- paste0('/',month_day_start,',',month_day_end)
  } else if (month_day_end != '') {
    strMonthsDays <- paste0('/',month_day_start,',',month_day_start)
  } else {
    strMonthsDays <- ''
  }

  if (accumulation_start_date != '') {
    strAccumulation <- paste0('&accumulationStartDay=',accumulation_start_date)
  } else {
    strAccumulation <- ''
  }

  if (length(exclude_years) != 0) {
    strexclude_years <- paste0('&excludeYears=',toString(exclude_years))
  } else {
    strexclude_years <- ''
  }

  gdd_methodString      <- paste0('?gddMethod=',gdd_method)
  gdd_base_tempString    <- paste0('&gddBaseTemp=',gdd_base_temp)
  gdd_min_boundaryString <- paste0('&gddMinBoundary=',gdd_min_boundary)
  gdd_max_boundaryString <- paste0('&gddMaxBoundary=',gdd_max_boundary)


  strYearsType <- paste0('/years')
  strYears <- paste0('/',year_start,',',year_end)
  url <- paste0(urlAddress, strBeg, strCoord, strType, strMonthsDays, strYearsType,
                    strYears,gdd_methodString,gdd_base_tempString,gdd_min_boundaryString,
                    gdd_max_boundaryString,strexclude_years,strAccumulation)

  doWeatherGet <- TRUE
  while (doWeatherGet == TRUE) {
    postbody = ''
    request <- httr::GET(url, body = postbody, httr::content_type('application/json'),
                         httr::add_headers(Authorization =paste0("Bearer ", awhereEnv75247$token)))

    a <- suppressMessages(httr::content(request, as = "text"))

    #The JSONLITE Serializer properly handles the JSON conversion

    x <- jsonlite::fromJSON(a,flatten = TRUE)

    if (grepl('API Access Expired',a)) {
      get_token(awhereEnv75247$uid,awhereEnv75247$secret)
    } else {
      doWeatherGet <- FALSE
    }
  }

  data <- data.table::as.data.table(x[[3]])

  varNames <- colnames(data)
  #This removes the non-data info returned with the JSON object
  data[,grep('_links',varNames) := NULL]
  data[,grep('.units',varNames) := NULL]

  currentNames <- copy(colnames(data))
  data[,field_id  := field_id]
  setcolorder(data,c('field_id',currentNames))

  return(as.data.frame(data))
}

#' @title agronomic_norms_latlng
#'
#' @description
#' \code{agronomic_norms_latlng} pulls agronomic norm data from aWhere's API based on latitude & longitude
#'
#' @details
#' This function allows you to calculate the averages for agronomic attributes
#' across any range of years for which data are available.  The data pulled includes norms for
#' growing degree days (GDDs), potential evapotranspiration (PET), Precipitation over
#' potential evapotranspiration (P/PET), accumulated GDDs, accumulated precipitation,
#' accumulated PET, and accumulated P/PET, along with the standard deviations
#' for these variables.  The data pulled is for the latitude and longitude identified.
#' Default units are returned by the API.
#'
#' The data returned in this function
#' allow you to compare this year or previous years to the long-term normals, calculated as
#' the average of those agronomic conditions on that day in that location over the years specified.
#'
#'
#'
#' @references http://developer.awhere.com/api/reference/agronomics/norms
#'
#' @param - latitude: the latitude of the requested location (double)
#' @param - longitude: the longitude of the requested locations (double)
#' @param - monthday_start: character string of the first month and day for which you want to retrieve data,
#'                          in the form: MM-DD.  This is the start of your date range. e.g. '07-01' (July 1) (required)
#' @param - monthday_end: character string of the last month and day for which you want to retrieve data,
#'                          in the form: MM-DD.  This is the end of your date range. e.g. '07-01' (July 1) (required)
#' @param - year_start: character string of the starting year (inclusive) of the range of years for which
#'                     you're calculating norms, in the form YYYY. e.g., 2008 (required)
#' @param - year_end: character string of the last year (inclusive) of the range of years for which
#'                     you're calculating norms, in the form YYYY. e.g., 2015 (required)
#' @param - exclude_year: character string of a year or years which you'd like to exclude from
#'                        your range of years on which to calculate norms. To exclude
#'                        multiple years, provide a vector of years. You must include
#'                       at least three years of data with which to calculate the norms. (optional)
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
#'
#' @import httr
#' @import data.table
#' @import lubridate
#' @import jsonlite
#'
#' @return dataframe of requested data for dates requested
#'
#' @examples

#' \dontrun{agronomic_norms_latlng(latitude = 39.8282, longitude = -98.5795,
#'                                 month_day_start = '07-01', month_day_end = '07-10',
#'                                 year_start = 2008, year_end = 2015,exclude_years = c(2010,2011),
#'                                 accumulation_start_date = '',gdd_method = 'standard',
#'                                 gdd_base_temp = 10,gdd_min_boundary = 10,gdd_max_boundary = 30)}


#' @export


agronomic_norms_latlng <- function(latitude, longitude, month_day_start, month_day_end,
                                   year_start, year_end,exclude_years = c(),
                                   accumulation_start_date = '',gdd_method = 'standard',gdd_base_temp = 10,
                                   gdd_min_boundary = 10, gdd_max_boundary = 30) {

  #############################################################
  #Checking Input Parameters
  checkCredentials()
  checkValidLatLong(latitude,longitude)
  checkGDDParams(gdd_method,gdd_base_temp,gdd_min_boundary,gdd_max_boundary)
  checkNormsStartEndDates(month_day_start,month_day_end)
  checkNormsYearsToRequest(year_start,year_end,month_day_start,month_day_end,exclude_years)
  checkAccumulationStartDateNorms(accumulation_start_date,month_day_start)

  # Create query

  urlAddress <- "https://api.awhere.com/v2/agronomics"

  strBeg <- paste0('/locations')
  strCoord <- paste0('/',latitude,',',longitude)
  strType <- paste0('/agronomicnorms')

  if (month_day_end != '') {
    strMonthsDays <- paste0('/',month_day_start,',',month_day_end)
  } else {
    strMonthsDays <- paste0('/',month_day_start,',',month_day_start)
  }

  if (accumulation_start_date != '') {
    strAccumulation <- paste0('&accumulationStartDay=',accumulation_start_date)
  } else {
    strAccumulation <- ''
  }

  if (length(exclude_years) != 0) {
    strexclude_years <- paste0('&excludeYears=',toString(exclude_years))
  } else {
    strexclude_years <- ''
  }

  gdd_methodString      <- paste0('?gddMethod=',gdd_method)
  gdd_base_tempString    <- paste0('&gddBaseTemp=',gdd_base_temp)
  gdd_min_boundaryString <- paste0('&gddMinBoundary=',gdd_min_boundary)
  gdd_max_boundaryString <- paste0('&gddMaxBoundary=',gdd_max_boundary)

  strYearsType <- paste0('/years')
  strYears <- paste0('/',year_start,',',year_end)
  url <- paste0(urlAddress, strBeg, strCoord, strType, strMonthsDays, strYearsType,
                    strYears,gdd_methodString,gdd_base_tempString,gdd_min_boundaryString,
                    gdd_max_boundaryString,strexclude_years,strAccumulation)

  doWeatherGet <- TRUE
  while (doWeatherGet == TRUE) {
    postbody = ''
    request <- httr::GET(url, body = postbody, httr::content_type('application/json'),
                         httr::add_headers(Authorization =paste0("Bearer ", awhereEnv75247$token)))

    a <- suppressMessages(content(request, as = "text"))

    if (grepl('API Access Expired',a)) {
      get_token(awhereEnv75247$uid,awhereEnv75247$secret)
    } else {
      doWeatherGet <- FALSE

      #The JSONLITE Serializer properly handles the JSON conversion
      x <- jsonlite::fromJSON(a,flatten = TRUE)
    }
  }

  data <- data.table::as.data.table(x[[3]])

  varNames <- colnames(data)
  #This removes the non-data info returned with the JSON object
  data[,grep('_links',varNames) := NULL]
  data[,grep('.units',varNames) := NULL]

  currentNames <- copy(colnames(data))
  data[,latitude  := latitude]
  data[,longitude := longitude]
  setcolorder(data,c('latitude','longitude',currentNames))

  return(as.data.frame(data))
}

