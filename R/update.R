#' @title Update Field
#'
#' @description
#' \code{update_field} To update details (Farm ID or FieldName) of a particular location in the aWhere API.
#'
#' @details
#' Fields are how you manage the locations for which you're tracking weather, agronomics,
#' models, and progress over growing seasons. By registering a field, you create a quick way
#' to consistently reference locations across all our APIs, and allow our modeling APIs
#' to better operate and tune to the conditions and status of each specific field. A Planting
#' is the record of a crop's season in a given field, and is where you tell the platform
#' about what is planted there and when it was planted.
#'
#' Occasionally, you may need to update a field location detail.
#' Only the Farm ID and Field Name can be updated at this time.
#' IMPORTANT - to look for a set of data points which need update, you can only update parameters
#'             one at a time:
#'                    - only farmId
#'                    - only name
#'
#' @param - field_id: the variable that will be used to search for the particular
#' @param - variable_search: the variable that will be used to search for the particular
#'                            locations that need information updated (character string).
#'                            Either "farmId" or "name"
#' @param - value_search: the value for the variable_search (character string)
#' @param - variable_update: the variable that needs to be updated (character string).
#'                            Either "farmId" or "name"
#' @param - value_update: the new value that will be placed (character string)
#'
#' @return - A message confirming the changes have been made
#'
#' @references http://developer.awhere.com/api/reference/fields/update-field
#'
#' @import httr
#' @import RCurl
#' @import jsonlite
#'
#' @examples
#' update_field( field_id = 'field123',
#' 				variable_search = 'farmId', value_search = 'farmA',
#'              variable_update = 'farmId', value_update = 'This is my territory')

#' @export

update_field <- function(field_id, variable_search, value_search, variable_update, value_update) {


    ## Creating the request

    url <- paste0("https://api.awhere.com/v2/fields/",field_id)

    postbody <- paste0('[{"op":"test","path":"/', variable_search, '","value":"', value_search, '"},
                         {"op":"replace","path":"/', variable_update, '","value":"', value_update, '"}]' )

    doWeatherGet <- TRUE
    while (doWeatherGet == TRUE) {
      ## Get data
      request <- PATCH(url, body = postbody, content_type('application/json'),
                       add_headers(Authorization = paste0("Bearer ", awhereEnv75247$token)))

      # Re formating the response recieved from API
      a <- content(request, as = "text")

      #The JSONLITE Serializer properly handles the JSON conversion

      if (any(grepl('API Access Expired',a)) == TRUE) {
        get_token(awhereEnv75247$uid,awhereEnv75247$secret)
      } else {
        doWeatherGet <- FALSE
      }
    }

    # Did the query work?

    if ((request$status_code %in% c(200,201,204)) == FALSE) { # status code = 200 means that the query worked
        warning('WARNING: Problem with Query')
        cat(paste0(x))
        return()
    } else {
      cat(paste0('Operation Complete'))
    }


}

#' @title Update Planting
#'
#' @description
#' \code{update_planting} To update details of a particular planting
#'
#' @details
#' Occasionally you will need to update a planting, changing the projections
#' or recording the end-of-season information for historical tracking and model tuning.
#' This API supports both partial and complete updates of plantings.  When updating an
#' entire planting, the whole object is replaced in the database. Any properties that
#' were previously set, and now are not, will be null'd. The required properties must
#' also be set even if they are changed.
#'
#' @param - planting_id: ID of planting to update
#' @param - field_id: ID of field to search for plantings within
#' @param - planting_date: new date to update as planting's plant date
#' @param - proj_yield_amount: new amount to update as planting's projected yield amount
#' @param - proj_yield_units: new units to update as planting's projected yield units
#' @param - proj_harvest_date: new projected harvest date to update as planting's projected harvest date
#' @param - yield_amount: new amount to update as planting's yield amount
#' @param - yield_units: new units to update as planting's yield units
#' @param - harvest_date: new actual harvest date to update as planting's harvest date
#'
#' @return - A message confirming the changes have been made
#'
#' @import httr
#' @import rjson
#' @import RJSONIO
#' @import RCurl
#'
#' @examples
#' update_planting("64322", "FieldA", "2016-02-01", "60", "Bushels")
#'
#' @export
update_planting <- function(field_id, planting_id = "", planting_date = "", proj_yield_amount = "", proj_yield_units = "",
                           proj_harvest_date = "", yield_amount = "", yield_units = "", harvest_date = "") {

  ## Error checking

  if(field_id == "") {
    stop("Field ID is required")
  }

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

  if(proj_yield_amount != "" & is.na(as.double(proj_yield_amount))) {
    stop("Yield amounts must be numeric")
  }

  if(yield_amount != "" & is.na(as.double(yield_amount))) {
    stop("Yield amounts must be numeric")
  }

  if(planting_date != "" & !is.Date(planting_date)){
    stop("Dates must be in 'YYYY-MM-DD' format")
  }
  if(proj_harvest_date != "" & !is.Date(proj_harvest_date)){
    stop("Dates must be in 'YYYY-MM-DD' format")
  }
  if(harvest_date != "" & !is.Date(harvest_date)){
    stop("Dates must be in 'YYYY-MM-DD' format")
  }


  ## Create postbody
  postbody <- c()

  i = 0

  if(planting_date != "") {
    i = i + 1
    postbody[i] <- paste0('{"op":"replace","path":"/planting_date","value":"', planting_date, '"}')
  }
  if(proj_yield_amount != "") {
    i = i + 1
    postbody[i] <- paste0('{"op":"replace","path":"/projections/yield/amount","value":', proj_yield_amount, '}')
  }
  if(proj_yield_units != "") {
    i = i + 1
    postbody[i] <- paste0('{"op":"replace","path":"/projections/yield/units","value":"', proj_yield_units, '"}')
  }
  if(proj_harvest_date != "") {
    i = i + 1
    postbody[i] <- paste0('{"op":"replace","path":"/projections/harvest_date","value":"', proj_harvest_date, '"}')
  }
  if(yield_amount != "") {
    i = i + 1
    postbody[i] <- paste0('{"op":"replace","path":"/yield/amount","value":', yield_amount, '}')
  }
  if(yield_units != "") {
    i = i + 1
    postbody[i] <- paste0('{"op":"replace","path":"/yield/units","value":"', yield_units, '"}')
  }
  if(harvest_date != "") {
    i = i + 1
    postbody[i] <- paste0('{"op":"replace","path":"/harvest_date","value":"', harvest_date, '"}')
  }

  if(i == 0) {
    stop("Must pass in a value to update")
  }

  body <- paste0("[", postbody[1])
  if(length(postbody) > 1) {
    for(i in 2:length(postbody)) {
      body <- paste0(body, ",", postbody[i])
    }
  }
  body <- paste0(body, "]")



  ## Creating the request

  url <- paste0("https://api.awhere.com/v2/agronomics/fields/", field_id, "/plantings/")

  if(planting_id == "") {
    url <- paste0(url, "current")
  } else {
    url <- paste0(url, planting_id)
  }


  ##Send request
  request <- PATCH(url, body = body, content_type('application/json'),
                   add_headers(Authorization = paste0("Bearer ", awhereEnv75247$token)))



  # Re formating the response recieved from API

  a <- content(request, as = "text")

  # Did the query work?

  if (request$status_code != 200) { # status code = 200 means that the query worked
    warning('WARNING: Problem with Query')
    cat(paste0(x))
    return()
  }


}

