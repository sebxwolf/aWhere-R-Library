#' @title CreatePlanting
#'
#' @description
#' \code{CreatePlanting} creates a planting in a field location in the aWhere platform for which you can request weather
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
#' @param - fieldId: an ID of your choosing (string)
#' @param - crop: cropId or crop name (string)
#' @param - plantingDate: date crop was planted in the field. Format as YYYY-MM-DD (string)
#' @param - projectedYieldAmount: amount of projected yield from planting (string)
#' @param - projectedYieldUnits: units of projected yield (string)
#' @param - projectedHarvestDate: projected harvest date at the start of the season. Format as YYYY-MM-DD (string)
#' @param - yieldAmount: actual yield (string)
#' @param - yieldUnits: units of actual yield (string)
#' @param - harvestDate: actual harvest date at end of season. Format as YYYY-MM-DD (string)
#'
#' @return - a print text that informs if the query succeded or not
#'
#' @references http://developer.awhere.com/api/reference/plantings/create
#'
#' @import httr
#' @import RCurl
#' @import jsonlite
#'
#' @examples
#' CreatePlanting('field123','corn','2015-10-25','100','Bushels', '2016-02-01','110','Bushels','2016-02-01')

#' @export

CreatePlanting <- function(fieldId, crop, plantingDate = "", projectedYieldAmount = "", projectedYieldUnits = "", projectedHarvestDate = "",
                        yieldAmount = "", yieldUnits = "", harvestDate = "") {

  ## Error checking for valid entries
  if(missing(fieldId)) {
    stop("Field ID is required")
  }

  if(missing(crop)) {
    stop("Crop is required")
  }

  if((projectedYieldAmount != "" & projectedYieldUnits == "") || (projectedYieldAmount == "" & projectedYieldUnits != "")) {
    stop("Must either have both projected yield amount and projected units, or neither")
  }

  if((yieldAmount != "" & yieldUnits == "") | (yieldAmount == "" & yieldUnits != "")) {
    stop("Must either have both yield amount and yield units, or neither")
  }


  url <- paste0("https://api.awhere.com/v2/agronomics/fields/", fieldId, "/plantings")

  postbody <- paste0('{',
                     '"crop":"', crop, '",',
                     '"plantingDate":"', plantingDate, '",',
                     '"projections":{',
                            '"yield":{',
                                '"amount":', projectedYieldAmount,',',
                                '"units":"', projectedYieldUnits, '"',
                            '},',
                            '"harvestDate":"', projectedHarvestDate, '"',
                     '},',
                     '"yield":{',
                            '"amount":', yieldAmount, ',',
                            '"units":"', yieldUnits, '"',
                     '},',
                     '"harvestDate":"', harvestDate, '"',
                     '}')


  doWeatherGet <- TRUE
  while (doWeatherGet == TRUE) {
    request <- POST(url, body=postbody, content_type('application/json'),
                    add_headers(Authorization = paste0("Bearer ", awhereEnv75247$token)))

    a <- content(request, as = "text")

    if (any(grepl('API Access Expired',a))) {
      GetAccessToken(awhereEnv75247$uid,awhereEnv75247$secret)
    } else {
      doWeatherGet <- FALSE
    }
  }

  parsedResponse <- unlist(strsplit(a,split = "\""))

  if (request$status_code %in% c(200,201,204) == FALSE) { # status code = 200 means that the query worked
    warning('WARNING: Problem with Query')
    cat(paste0(parsedResponse))
    return()
  } else {
    cat(paste0('Operation Complete \n'))
  }

}
