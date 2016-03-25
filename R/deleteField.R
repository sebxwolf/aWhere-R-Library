#' @title DeleteField
#'
#' @description
#' \code{DeleteField} deletes a fieldId for a location in the aWhere platform for which you can request weather
#'
#' @details
#' This script deletes a field location in the aWhere platform.
#' This API is a "hard delete" - the field record should actually be deleted from the system.
#' The delete should cascade, if there are associated records to a field, they are deleted as well.
#' This applies when we design/implement "Plantings" API.
#'
#' @param - fieldId: an ID of your choosing (string)
#'
#' @return - a print text that informs if the query succeded or not
#'
#' @import httr
#' @import RCurl
#'
#' @examples
#' DeleteField("field123")
#' DeleteField("field456")
#'
#' @export


DeleteField <- function(fieldId) {

  url <- paste0("https://api.awhere.com/v2/fields/", fieldId)

  postbody <- paste0('{', fieldId, '}');

  doWeatherGet = TRUE
  while (doWeatherGet == TRUE) {
    ## Get data
    request <- DELETE(url, body=postbody, content_type('application/json'),
                      add_headers(Authorization = paste0("Bearer ", awhereEnv75247$token)), curl = getCurlHandle())

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
