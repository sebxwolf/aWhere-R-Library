#' @title Update Field
#'
#' @description
#' \code{update_field} To update details (Farm ID or FieldName) of a particular location in the aWhere API.
#'
#' @details
#' Fields are the easiest way to manage locations in the aWhere APIs, providing an easy reference 
#' for tracking weather, agronomics, models, and progress over growing seasons. Once a field is 
#' registered, plantings can also be registered for that field with specific information about the
#' crop planted at that location, the date of planting, and other optional information.
#'
#' Occasionally, you may need to update the details of your field. At this time, only the farm ID, 
#' field Name, and number of acres can be updated using this function. Field details can only be 
#' updated one variable at a time, for one field at a time. If you need to update multiple fields or
#' multiple variables associated with a field, please pass commands sequentially.
#'
#' @param - field_id: the unique field ID for the field you want to update (character string) (required)
#' @param - variable_update: the variable that needs to be updated, either "farmId", "name", or "acres" 
#' (character string) (required)
#' @param - value_update: the new value for variable_update, to replace the existing value. The existing 
#' value can be found using get_fields("field_id") (character string) (required)
#'
#' @return - A message confirming the changes have been made
#'
#' @references http://developer.awhere.com/api/reference/fields/update-field
#'
#' @import httr
#'
#' @examples
#' \dontrun{update_field(fieldIDOrName = 'field123',
#'        variable_update = 'farmId', value_update = 'This is my territory')}

#' @export

update_field <- function(field_id, variable_update, value_update) {

  checkCredentials()
  checkValidField(field_id)

  ## Creating the request

  url <- paste0("https://api.awhere.com/v2/fields/",field_id)

  postbody <- paste0('[{"op":"replace","path":"/', variable_update, '","value":"', value_update, '"}]')

  doWeatherGet <- TRUE
  while (doWeatherGet == TRUE) {
    ## Get data
    request <- httr::PATCH(url, body = postbody, httr::content_type('application/json'),
                           httr:: add_headers(Authorization = paste0("Bearer ", awhereEnv75247$token)))

    # Re formating the response recieved from API
    a <- httr::content(request, as = "text")

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
#'
#' @examples
#' \dontrun{update_planting("field123", "64322", harvest_date = "2016-02-01", yield_amount = "60", yield_units = "Bushels")}
#'
#' @export
update_planting <- function(field_id, planting_id, planting_date = "", proj_yield_amount = "",
                            proj_yield_units = "", proj_harvest_date = "", yield_amount = "", yield_units = "", harvest_date = "") {

  checkCredentials()
  checkValidField(field_id)

  ## Error checking

  if(field_id == "") {
    stop("Field ID is required")
  }

  if(proj_yield_amount != "" & is.na(as.double(proj_yield_amount))) {
    stop("Yield amounts must be numeric")
  }

  if(yield_amount != "" & is.na(as.double(yield_amount))) {
    stop("Yield amounts must be numeric")
  }

  if(planting_date != "" & !is.Date(ymd(planting_date))){
    stop("Dates must be in 'YYYY-MM-DD' format")
  }

  if(proj_harvest_date != "" & !is.Date(ymd(proj_harvest_date))){
    stop("Dates must be in 'YYYY-MM-DD' format")
  }

  if(harvest_date != "" & !is.Date(ymd(harvest_date))){
    stop("Dates must be in 'YYYY-MM-DD' format")
  }


  ## Create postbody
  postbody <- c()

  i = 0

  if(planting_date != "") {
    i = i + 1
    postbody[i] <- paste0('{"op":"replace","path":"/plantingDate","value":"', planting_date, '"}')
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
    postbody[i] <- paste0('{"op":"replace","path":"/projections/harvestDate","value":"', proj_harvest_date, '"}')
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
    postbody[i] <- paste0('{"op":"replace","path":"/harvestDate","value":"', harvest_date, '"}')
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
  request <- httr::PATCH(url, body = body, httr::content_type('application/json'),
                         httr::add_headers(Authorization = paste0("Bearer ", awhereEnv75247$token)))



  # Re formating the response recieved from API

  a <- httr::content(request, as = "text")

  # Did the query work?

  if (request$status_code != 200) { # status code = 200 means that the query worked
    warning('WARNING: Problem with Query')
    cat(paste0(x))
    return()
  }


}

