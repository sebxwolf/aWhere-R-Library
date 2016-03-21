#' @title updateField
#'
#' @description
#' \code{updateField} To update details (Farm ID or FieldName) of a particular location in the aWhere API.
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
#' @param - fieldId: the variable that will be used to search for the particular
#' @param - variableToSearch: the variable that will be used to search for the particular
#'                            locations that need information updated (character string).
#'                            Either "farmId" or "name"
#' @param - valueToSearch: the value for the variableToSearch (character string)
#' @param - variableToUpdate: the variable that needs to be updated (character string).
#'                            Either "farmId" or "name"
#' @param - valueToUpdate: the new value that will be placed (character string)
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
#' updateField( fieldId = 'field123', 
#' 				variableToSearch = 'farmId', valueToSearch = 'farmA',
#'              variableToChange = 'farmId', valueToChange = 'This is my territory')

#' @export

UpdateField <- function(fieldId, variableToSearch, valueToSearch, variableToChange, valueToChange) {


    ## Creating the request

    url <- paste0("https://api.awhere.com/v2/fields/",fieldId)

    postbody <- paste0('[{"op":"test","path":"/', variableToSearch, '","value":"', valueToSearch, '"},
                         {"op":"replace","path":"/', variableToChange, '","value":"', valueToChange, '"}]' )

    doWeatherGet <- TRUE
    while (doWeatherGet == TRUE) {
      ## Get data
      request <- PATCH(url, body = postbody, content_type('application/json'),
                       add_headers(Authorization = paste0("Bearer ", awhereEnv75247$token)))

      # Re formating the response recieved from API
      a <- content(request, as = "text")

      #The JSONLITE Serializer properly handles the JSON conversion

      if (any(grepl('API Access Expired',a)) == TRUE) {
        GetAccessToken(awhereEnv75247$uid,awhereEnv75247$secret)
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
