#' @title GetFields
#'
#' @description
#' \code{GetFields} calls Get Field Locations Endpoint of API
#'
#' @details
#' Fields are how you manage the locations for which you're tracking weather, agronomics,
#' models, and progress over growing seasons. By registering a field, you create a quick way
#' to consistently reference locations across all our APIs, and allow our modeling APIs
#' to better operate and tune to the conditions and status of each specific field. A Planting
#' is the record of a crop's season in a given field, and is where you tell the platform
#' about what is planted there and when it was planted.
#'
#' Before using our other APIs you'll need to register the field locations if using the Fields Name construct.
#' This is a one-time step. Every field has an ID that you define, plus a latitude and longitude.
#' Fields are universal across all our APIs, and as you provide information about a field, some APIs
#' (such as agronomics and models) can leverage that detail internally to more easily and seamlessly
#' calculate information for you.
#'
#' @references http://developer.awhere.com/api/reference/fields/get-fields
#'
#' @param - fieldId: Either a specified field name (as a string) to retrieve info on only that field
#'                   or an empty string to retrieve info on all fields
#'
#' @return - data: data.table containing information about requested field(s)
#'
#' @import httr
#' @import RCurl
#' @import jsonlite
#'
#' @examples
#' GetFields('field123')

#' @export

GetFields <- function(fieldId = "") {
  ## Create Request
  url <- "https://api.awhere.com/v2/fields/"

  if(fieldId != "") {
    url <- paste0(url, fieldId)
  }

  doWeatherGet <- TRUE
  while (doWeatherGet == TRUE) {
    ## Get data
    request <- GET(url,
                   content_type('application/json'),
                   add_headers(Authorization =
                                 paste0("Bearer ", awhereEnv75247$token)))

    a <- content(request)

    #The JSONLITE Serializer propely handles the JSON conversion

    if (any(grepl('API Access Expired',a)) == TRUE) {
      GetAccessToken(awhereEnv75247$uid,awhereEnv75247$secret)
    } else {
      doWeatherGet <- FALSE
    }
  }

  ## Create & fill data frame

  if(fieldId == "") {
    data <- as.data.frame(do.call(rbind, lapply(a$fields, rbind)))[, c(1:5)]
    data <- cbind(data, do.call(rbind, lapply(data$centerPoint, rbind)))
    data$centerPoint <- NULL
    colnames(data) <- c("fieldName", "Acres", "farmId", "fieldId", "Latitude", "Longitude")

    data <- as.matrix(data)
    data[sapply(data, is.null)] <- NA
    data <- as.data.frame(data)
    for(i in 1:ncol(data)) {
      data[,i] <- do.call(rbind, lapply(data[,i], rbind))[,1]
    }

  } else {
    a[sapply(a, is.null)] <- NA
    data <- data.frame(fieldName = unlist(a$name), acres = unlist(a$acres), latitude = unlist(a$centerPoint$latitude),
                       longitude = unlist(a$centerPoint$longitude), farmId = unlist(a$farmId), fieldId = unlist(a$id))
  }

  if(nrow(data) == 0) {
    stop(a$simpleMessage)
  } else {
    return(as.data.table(data))
  }

}
