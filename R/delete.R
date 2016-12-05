#' @title Delete Field
#'
#' @description
#' \code{delete_field} deletes a field_id for a location in the aWhere platform for which you can request weather
#'
#' @details
#' This script deletes a field location in the aWhere platform.
#' This API is a "hard delete" - the field record should actually be deleted from the system.
#' The delete should cascade, if there are associated records to a field, they are deleted as well.
#' This applies when we design/implement "Plantings" API.
#'
#' @param - field_id: an ID of your choosing (string)
#'
#' @return - a print text that informs if the query succeded or not
#' @importFrom RCurl getCurlHandle
#' @examples
#' \dontrun{delete_field("field123")
#' delete_field("field456")
#' }
#' @export

delete_field <- function(field_id) {

  url <- paste0("https://api.awhere.com/v2/fields/", field_id)

  postbody <- paste0('{', field_id, '}');

  doWeatherGet = TRUE
  while (doWeatherGet == TRUE) {
    ## Get data
    request <- httr::DELETE(url, body=postbody, httr::content_type('application/json'),
                            httr::add_headers(Authorization = paste0("Bearer ", awhereEnv75247$token)), curl = getCurlHandle())

    a <- httr::content(request, as = "text")
    parsedResponse <- unlist(strsplit(a,split = "\""))

    #The JSONLITE Serializer propely handles the JSON conversion

    if (any(grepl('API Access Expired',a)) == TRUE) {
      get_token(awhereEnv75247$uid,awhereEnv75247$secret)
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

#' @title Delete Planting
#'
#' @description
#' \code{delete_planting} deletes a planting associated with a specific field_id in the aWhere platform
#'
#' @details
#' The aWhere API only references the most recent planting when calculating agronomics and running
#' models, but if you want to keep your planting records clean for reporting and historical tracking
#' purposes you can delete errant or incorrect plantings.
#'
#' @param - field_id: the ID of the field for which you want to delete an associated planting (string)
#' @param - planting_id: The planting Id that you want to delete.  You can also use "current" to delete the most recent planting (string)
#'
#' @return - a print text that informs if the query succeded or not
#'
#' @examples
#' \dontrun{delete_planting("field123",'133972')}

#' @export

delete_planting <- function(field_id,planting_id) {

  url <- paste0("https://api.awhere.com/v2/agronomics/fields/", field_id,'/plantings/',planting_id)

  doWeatherGet = TRUE
  while (doWeatherGet == TRUE) {
    ## Get data
    request <- httr::DELETE(url, httr::add_headers(Authorization = paste0("Bearer ", awhereEnv75247$token)))

    a <- httr::content(request, as = "text")
    parsedResponse <- unlist(strsplit(a,split = "\""))

    #The JSONLITE Serializer properly handles the JSON conversion

    if (any(grepl('API Access Expired',a)) == TRUE) {
      get_token(awhereEnv75247$uid,awhereEnv75247$secret)
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

