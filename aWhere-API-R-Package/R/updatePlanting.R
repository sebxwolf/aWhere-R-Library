#' @title updatePlanting
#'
#' @description
#' \code{updatePlanting} To update details of a particular planting
#'
#' @details
#' Occasionally you will need to update a planting, changing the projections
#' or recording the end-of-season information for historical tracking and model tuning.
#' This API supports both partial and complete updates of plantings.  When updating an
#' entire planting, the whole object is replaced in the database. Any properties that
#' were previously set, and now are not, will be null'd. The required properties must
#' also be set even if they are changed.
#'
#' @param - plantingId: ID of planting to update
#' @param - fieldId: ID of field to search for plantings within
#' @param - plantingDate: new date to update as planting's plant date
#' @param - projectedYieldAmount: new amount to update as planting's projected yield amount
#' @param - projectedYieldUnits: new units to update as planting's projected yield units
#' @param - projectedHarvestDate: new projected harvest date to update as planting's projected harvest date
#' @param - yieldAmount: new amount to update as planting's yield amount
#' @param - yieldUnits: new units to update as planting's yield units
#' @param - harvestDate: new actual harvest date to update as planting's harvest date
#'
#' @return - A message confirming the changes have been made
#'
#' @import httr
#' @import rjson
#' @import RJSONIO
#' @import RCurl
#'
#' @examples
#' updateField("64322", "FieldA", "2016-02-01", "60", "Bushels")
#'
#' @export

updatePlanting <- function(fieldId, plantingId = "", plantingDate = "", projectedYieldAmount = "", projectedYieldUnits = "",
                           projectedHarvestDate = "", yieldAmount = "", yieldUnits = "", harvestDate = "") {

  ## Error checking

  if(fieldId == "") {
    stop("Field ID is required")
  }

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

  if(projectedYieldAmount != "" & is.na(as.double(projectedYieldAmount))) {
    stop("Yield amounts must be numeric")
  }

  if(yieldAmount != "" & is.na(as.double(yieldAmount))) {
    stop("Yield amounts must be numeric")
  }

  if(plantingDate != "" & !is.Date(plantingDate)){
    stop("Dates must be in 'YYYY-MM-DD' format")
  }
  if(projectedHarvestDate != "" & !is.Date(projectedHarvestDate)){
    stop("Dates must be in 'YYYY-MM-DD' format")
  }
  if(harvestDate != "" & !is.Date(harvestDate)){
    stop("Dates must be in 'YYYY-MM-DD' format")
  }


  ## Create postbody
  postbody <- c()

  i = 0

  if(plantingDate != "") {
    i = i + 1
    postbody[i] <- paste0('{"op":"replace","path":"/plantingDate","value":"', plantingDate, '"}')
  }
  if(projectedYieldAmount != "") {
    i = i + 1
    postbody[i] <- paste0('{"op":"replace","path":"/projections/yield/amount","value":', projectedYieldAmount, '}')
  }
  if(projectedYieldUnits != "") {
    i = i + 1
    postbody[i] <- paste0('{"op":"replace","path":"/projections/yield/units","value":"', projectedYieldUnits, '"}')
  }
  if(projectedHarvestDate != "") {
    i = i + 1
    postbody[i] <- paste0('{"op":"replace","path":"/projections/harvestDate","value":"', projectedHarvestDate, '"}')
  }
  if(yieldAmount != "") {
    i = i + 1
    postbody[i] <- paste0('{"op":"replace","path":"/yield/amount","value":', yieldAmount, '}')
  }
  if(yieldUnits != "") {
    i = i + 1
    postbody[i] <- paste0('{"op":"replace","path":"/yield/units","value":"', yieldUnits, '"}')
  }
  if(harvestDate != "") {
    i = i + 1
    postbody[i] <- paste0('{"op":"replace","path":"/harvestDate","value":"', harvestDate, '"}')
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

  url <- paste0("https://api.awhere.com/v2/agronomics/fields/", fieldId, "/plantings/")

  if(plantingId == "") {
    url <- paste0(url, "current")
  } else {
    url <- paste0(url, plantingId)
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
