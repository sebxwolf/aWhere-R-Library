#' @title CreateField
#'
#' @description
#' \code{CreateField} This API will register a field location in the aWhere platform. This is a one-time operation for each field.
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
#' @param - fieldId: an ID of your choosing (string)
#' @param - latitude: the latitude of the field location in decimal format. (string)
#' @param - longitude: the longitude of the field location in decimal format (string)
#' @param - farmId: an arbitrary ID for the farm to which this field belongs (string)
#' @param - fieldName: a name of the location (optional -string)
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
#' CreateField("field123","39.8282","-98.5795","farmA","Some Field Location","100")
#' CreateField("field456","40.8282","-100.5795","farmA","Some Field Location","100")

#' @export

CreateField <- function(fieldId, latitude, longitude, farmId, fieldName = "", acres = "") {

  #############################################################
  #Checking Input Parameters
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

  if (acres != "") {
    if (suppressWarnings(is.na(as.double(acres))) == TRUE) {
      warning('The entered acres Value is not valid. Please correct\n')
      return()
    }
  }

  fieldId <- gsub(' ','_',fieldId)
  farmId <- gsub(' ','_',farmId)
  # fieldName <- gsub(' ','_',fieldName)

  ## Create Request


  url <- "https://api.awhere.com/v2/fields"

  postbody <- paste0('{"id":"', fieldId, '",',
                     '"centerPoint":{"latitude":', latitude, ',"longitude":', longitude, '}',
                     ',"farmId":"', farmId, '"')
  if(fieldName != "") {
    postbody <- paste0(postbody, ',"name":"', fieldName, '"')
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
      GetAccessToken(awhereEnv75247$uid,awhereEnv75247$secret)
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
