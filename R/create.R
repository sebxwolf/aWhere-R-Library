#' @title Create Field
#'
#' @description
#' \code{create_field} This API will register a field location in the aWhere platform. This is a one-time operation for each field.
#'
#' @details
#' Fields are how you manage the locations for which you're tracking weather, agronomics,
#' models, and progress over growing seasons. By registering a field, you create a quick way
#' to consistently reference locations across all our APIs, and allow our modeling APIs
#' to better operate and tune to the conditions and status of each specific field. A Planting
#' is the record of a crop's season in a given field, and is where you tell the platform
#' about what is planted there and when it was planted.
#'
#' Creating a field registers the location with the aWhere system, making it easier to reference
#' and track your locations as well as run agronomics and models automatically. You
#' only need to create a field once, after which point you can reference it by the ID you create
#' (you'll use this ID in virtually every URI endpoint in our system).
#'
#' All Spaces will be converted to underscores to conform with the requirements of the API
#'
#' @param - field_id: an ID of your choosing (string)
#' @param - latitude: the latitude of the field location in decimal format. (string)
#' @param - longitude: the longitude of the field location in decimal format (string)
#' @param - farmd: an arbitrary ID for the farm to which this field belongs (string)
#' @param - field_name: a name of the location (optional -string)
#' @param - acres: the acres of the field (optional -string)
#'
#' @return - printed text that informs if the query succeeded or not
#'
#' @references http://developer.awhere.com/api/reference/fields/create-field
#'
#' @import httr
#' @import RCurl
#' @import jsonlite
#'
#' @examples
#' create_field("field123","39.8282","-98.5795","farmA","Some Field Location","100")
#' create_field("field456","40.8282","-100.5795","farmA","Some Field Location","100")

#' @export
create_field <- function(field_id, latitude, longitude, farm_id, field_name = "", acres = "") {

  #############################################################
  #Checking Input Parameters
  if (exists('awhereEnv75247') == FALSE) {
    warning('Please Run the Command \'get_token()\' and then retry running command. \n')
    return()
  }

  if (exists('uid', envir = awhereEnv75247) == FALSE |
      exists('secret', envir = awhereEnv75247) == FALSE |
      exists('token', envir = awhereEnv75247) == FALSE) {
    warning('Please Run the Command \'get_token()\' and then retry running command. \n')
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

  if (acres != "") {
    if (suppressWarnings(is.na(as.double(acres))) == TRUE) {
      warning('The entered acres Value is not valid. Please correct\n')
      return()
    }
  }

  field_id <- gsub(' ','_',field_id)
  farm_id <- gsub(' ','_',farm_id)
  # field_name <- gsub(' ','_',field_name)

  ## Create Request
  url <- "https://api.awhere.com/v2/fields"

  postbody <- paste0('{"id":"', field_id, '",',
                     '"centerPoint":{"latitude":', latitude, ',"longitude":', longitude, '}',
                     ',"farmId":"', farm_id, '"')
  if(field_name != "") {
    postbody <- paste0(postbody, ',"name":"', field_name, '"')
  }
  if(acres != "") {
    postbody <- paste0(postbody, ',"acres":', acres)
  }
  postbody <- paste0(postbody, '}')


  doWeatherGet <- TRUE
  while (doWeatherGet == TRUE) {
    request <- POST(url, body=postbody, content_type('application/json'),
                    add_headers(Authorization = paste0("Bearer ", awhereEnv75247$token)))

    a <- content(request, as = "text")

    #The JSONLITE Serializer properly handles the JSON conversion

    x <- jsonlite::fromJSON(a,flatten = TRUE)

    if (grepl('API Access Expired',a)) {
      get_token(awhereEnv75247$uid,awhereEnv75247$secret)
    } else {
      doWeatherGet <- FALSE
    }
  }

  parsedResponse <- unlist(strsplit(a,split = "\""))

  if ((request$status_code %in% c(201)) == FALSE) { # status code = 200 means that the query worked
    warning('WARNING: Problem with Query')
    cat(paste0(parsedResponse))
    return()
  } else {
    cat(paste0('Operation Complete \n'))
  }

}


#' @title Create Planting
#'
#' @description
#' \code{create_planting} creates a planting in a field location in the aWhere platform for which you can request weather
#'
#' @details
#' Fields are how you manage the locations for which you're tracking weather, agronomics,
#' models, and progress over growing seasons. By registering a field, you create a quick way
#' to consistently reference locations across all our APIs, and allow our modeling APIs
#' to better operate and tune to the conditions and status of each specific field. A Planting
#' is the record of a crop's season in a given field, and is where you tell the platform
#' about what is planted there and when it was planted.
#'
#' Creating a planting will provide the aWhere platform the information needed to run models
#' and more efficiently calculate agronomic values. You can also use these properties to record
#' projections for the field, like yield or harvest date, to track the success of a field over
#' the course of a growing season. Recording projected and actual yield and harvest date also helps
#' aWhere tune the models for even greater accuracy.
#'
#' There can only be one active planting per field. You can create multiple plantings per field
#' but only the most recent one will be considered the "current" one. Use this functionality to
#' create historical records if you have them.
#'
#' When creating a planting, you must specify the crop and planting date.
#' The crop must be an option from the Crops API; but there is also a short cut where if you don't
#' know or want to use a specific crop ID, you can simply specify the crop name, such as "corn" or
#' "wheat" and the the API will select the default for that category.
#'
#' This script creates a planting in a field location in the aWhere platform. By setting an Id you can retrieve the weather
#' and agronomics for that location in all the other APIs. The planting ID corresponds to a planting within a field.
#'
#' @param - field_id: an ID of your choosing (string)
#' @param - crop: cropId or crop name (string)
#' @param - planting_date: date crop was planted in the field. Format as YYYY-MM-DD (string)
#' @param - proj_yield_amount: amount of projected yield from planting (string)
#' @param - proj_yield_units: units of projected yield (string)
#' @param - proj_harvest_date: projected harvest date at the start of the season. Format as YYYY-MM-DD (string)
#' @param - yield_amount: actual yield (string)
#' @param - yield_units: units of actual yield (string)
#' @param - harvest_date: actual harvest date at end of season. Format as YYYY-MM-DD (string)
#'
#' @return - system generated planting id along with a print text that informs if the query succeeded or not 
#'
#' @references http://developer.awhere.com/api/reference/plantings/create
#'
#' @import httr
#' @import RCurl
#' @import jsonlite
#'
#' @examples
#' create_planting('field123','corn','2015-10-25','100','Bushels', '2016-02-01','110','Bushels','2016-02-01')

#' @export

create_planting <- function(field_id, crop, planting_date = "", proj_yield_amount = "", proj_yield_units = "", proj_harvest_date = "",
                            yield_amount = "", yield_units = "", harvest_date = "") {

  ## Error checking for valid entries
  if(missing(field_id)) {
    stop("Field ID is required")
  }

  if(missing(crop)) {
    stop("Crop is required")
  }

  if((proj_yield_amount != "" & proj_yield_units == "") || (proj_yield_amount == "" & proj_yield_units != "")) {
    stop("Must either have both projected yield amount and projected units, or neither")
  }

  if((yield_amount != "" & yield_units == "") | (yield_amount == "" & yield_units != "")) {
    stop("Must either have both yield amount and yield units, or neither")
  }

  if(planting_date == "") {
    planting_date <- as.character(Sys.Date())
  }

  url <- paste0("https://api.awhere.com/v2/agronomics/fields/", field_id, "/plantings")


  postbody <- paste0('{',
                     '"crop":"', crop, '",',
                     '"plantingDate":"', planting_date, '"')
  if(proj_yield_amount != "" | proj_harvest_date != "") {
    postbody <- paste0(postbody, ',"projections":{')
    if(proj_yield_amount != "") {
      postbody <- paste0(postbody, '"yield":{',
                         '"amount":', proj_yield_amount,',',
                         '"units":"', proj_yield_units, '"}')
      if(proj_harvest_date != "") {
        postbody <- paste0(postbody, ",")
      }
    }
    if(proj_harvest_date != "") {
      postbody <- paste0(postbody, '"harvest_date":"', proj_harvest_date, '"',
                         '}')
    }
  }
  if(yield_amount != "") {
    postbody <- paste0(postbody, ',"yield":{',
                       '"amount":', yield_amount, ',',
                       '"units":"', yield_units, '"',
                       '}')
  }
  if(harvest_date != "") {
    postbody <- paste0(postbody, '"harvest_date":"', harvest_date, '"')
  }

  postbody <- paste0(postbody, '}')


  doWeatherGet <- TRUE
  while (doWeatherGet == TRUE) {
    request <- POST(url, body=postbody, content_type('application/json'),
                    add_headers(Authorization = paste0("Bearer ", awhereEnv75247$token)))

    a <- content(request)

    if (any(grepl('API Access Expired',a))) {
      get_token(awhereEnv75247$uid,awhereEnv75247$secret)
    } else {
      doWeatherGet <- FALSE
    }
  }

  # parsedResponse <- unlist(strsplit(a,split = "\""))


  if (!is.null(a$statusCode)) { # status code = 200 means that the query worked
    warning('WARNING: Problem with Query')
    cat(paste0(a$detailedMessage))
  } else {
    cat(paste0('Operation Complete \n Planting ID: ', a$id))
    a$id
  }

}
