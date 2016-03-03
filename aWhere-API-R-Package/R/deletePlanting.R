#' @title DeletePlanting
#'
#' @description
#' \code{DeletePlanting} deletes a planting associated with a specific FieldId in the aWhere platform
#'
#' @details
#' The aWhere API only references the most recent planting when calculating agronomics and running
#' models, but if you want to keep your planting records clean for reporting and historical tracking
#' purposes you can delete errant or incorrect plantings.
#'
#' @param - fieldId: the ID of the field for which you want to delete an associated planting (string)
#' @param - plantingId: The planting Id that you want to delete.  You can also use "current" to delete the most recent planting (string)
#'
#' @return - a print text that informs if the query succeded or not
#'
#' @import httr
#' @import RCurl
#'
#' @examples
#' DeletePlanting("field123",'64921')

#' @export

DeletePlanting <- function(fieldId,plantingId) {

  url <- paste0("https://api.awhere.com/v2/agronomics/fields/", fieldId,'/plantings/',plantingId)

  doWeatherGet = TRUE
  while (doWeatherGet == TRUE) {
    ## Get data
    request <- DELETE(url,add_headers(Authorization = paste0("Bearer ", awhereEnv75247$token)))

    a <- content(request, as = "text")
    parsedResponse <- unlist(strsplit(a,split = "\""))

    #The JSONLITE Serializer propely handles the JSON conversion

    if (any(grepl('API Access Expired',a)) == TRUE) {
      GetAccessToken(awhereEnv75247$uid,awhereEnv75247$secret)
    } else {
      doWeatherGet <- FALSE
    }
  }

  if ((request$status_code %in% c(200,204)) == FALSE) { # status code = 200 means that the query worked. I don't know what 204 is but it does delete the field
      warning('WARNING: Problem with Query')
      cat(paste0(parsedResponse))
      return()
  } else {
      cat(paste0('Operation Complete'))
  }

}
