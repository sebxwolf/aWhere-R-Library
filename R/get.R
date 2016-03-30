#' @title Get Fields
#'
#' @description
#' \code{get_fields} calls Get Field Locations Endpoint of API
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
#' @param - field_id: Either a specified field name (as a string) to retrieve info on only that field
#'                   or an empty string to retrieve info on all fields
#'
#' @return - data: data.table containing information about requested field(s)
#'
#' @import httr
#' @import RCurl
#' @import jsonlite
#'
#' @examples
#' get_fields('field123')

#' @export

get_fields <- function(field_id = "") {
  ## Create Request
  url <- "https://api.awhere.com/v2/fields/"

  if(field_id != "") {
    url <- paste0(url, field_id)
  }

  doWeatherGet <- TRUE
  while (doWeatherGet == TRUE) {
    ## Get data
    request <- GET(url,
                   content_type('application/json'),
                   add_headers(Authorization =
                                 paste0("Bearer ", awhereEnv75247$token)))

    a <- suppressMessages(content(request))

    #The JSONLITE Serializer propely handles the JSON conversion

    if (any(grepl('API Access Expired',a)) == TRUE) {
      get_token(awhereEnv75247$uid,awhereEnv75247$secret)
    } else {
      doWeatherGet <- FALSE
    }
  }

  ## Create & fill data frame

  if(field_id == "") {
    data <- as.data.frame(do.call(rbind, lapply(a$fields, rbind)))[, c(1:5)]
    data <- cbind(data, do.call(rbind, lapply(data$centerPoint, rbind)))
    data$centerPoint <- NULL
    colnames(data) <- c("fieldName", "Acres", "farmId", "field_id", "Latitude", "Longitude")

    data <- as.matrix(data)
    data[sapply(data, is.null)] <- NA
    data <- as.data.frame(data)
    for(i in 1:ncol(data)) {
      data[,i] <- do.call(rbind, lapply(data[,i], rbind))[,1]
    }

  } else {
    a[sapply(a, is.null)] <- NA
    data <- data.frame(fieldName = unlist(a$name), acres = unlist(a$acres), latitude = unlist(a$centerPoint$latitude),
                       longitude = unlist(a$centerPoint$longitude), farmId = unlist(a$farmId), field_id = unlist(a$id))
  }

  if(nrow(data) == 0) {
    stop(a$simpleMessage)
  } else {
    return(as.data.frame(data))
  }

}


#' @title Get Planting
#'
#' @description
#' \code{get_planting} Gets a planting or list of plantings in the aWhere platform for which you can request weather
#'
#' @details
#' Fields are how you manage the locations for which you're tracking weather, agronomics,
#' models, and progress over growing seasons. By registering a field, you create a quick way
#' to consistently reference locations across all our APIs, and allow our modeling APIs
#' to better operate and tune to the conditions and status of each specific field. A Planting
#' is the record of a crop's season in a given field, and is where you tell the platform
#' about what is planted there and when it was planted.
#'
#' Plantings are the way to manage crop and field activity in the aWhere API. Use this
#' API to record the type of crop, planting date, projections, and actuals to get the
#' most out of our more advanced APIs. Over time, this API also enables historical metrics
#' for any given field in the platform. In this function by setting an Id you can retrieve the weather
#' and agronomics for that location in all the other APIs.
#'
#' @param - field_id: a field ID to look within (string)
#' @param - planting_id: a planting ID to look for (string)
#' @param - current: whether to just get current plantings(T) or include historical plantings(F).
#'                   To get most recent planting record for a field, set current to TRUE and do not pass in a planting_id (boolean)
#'
#' @return - data: data.table containing information about requested field(s)
#'
#' @references http://developer.awhere.com/api/reference/plantings/get-plantings
#'
#' @import httr
#' @import RCurl
#' @import jsonlite
#'
#' @examples
#' GetField(1234)

#' @export

get_planting <- function(field_id = "", planting_id = "", current = F) {


  ## Create Request
  url <- "https://api.awhere.com/v2/agronomics/"

  if(field_id != "") {
    url <- paste0(url, "fields/", field_id, "/plantings")
  } else {
    url <- paste0(url, "plantings/")
  }

  if(planting_id != "") {
    url <- paste0(url, planting_id)
  }

  if(current) {
    url <- paste0(url, "current")
  }

  doWeatherGet <- TRUE
  while (doWeatherGet == TRUE) {
    request <- GET(url,
                   content_type('application/json'),
                   add_headers(Authorization =
                                 paste0("Bearer ", awhereEnv75247$token)))

    a <- suppressMessages(content(request))

    if (any(grepl('API Access Expired',a))) {
      get_token(awhereEnv75247$uid,awhereEnv75247$secret)
    } else {
      doWeatherGet <- FALSE
    }
  }

  ## Get data

  ## Create & fill data frame
  if(is.null(a$statusCode)) {
    if(planting_id == "") {
      data <- as.data.frame(do.call(rbind, lapply(a$plantings, rbind)))[, c(1:7)]
      data <- cbind(data, do.call(rbind, lapply(data$yield, rbind)))
      data$yield <- NULL
      data <- cbind(data, do.call(rbind, lapply(data$projections, rbind)))
      data <- cbind(data, do.call(rbind, lapply(data$yield, rbind)))
      data$yield <- NULL
      data$projections <- NULL

      colnames(data) <- c("planting_id", "crop", "field_id", "plantingDate", "actualHarvestDate", "yieldAmount", "yieldUnits",
                          "projectedHarvestDate", "projectedYieldAmount", "projectedYieldUnits")
      data <- as.matrix(data)
      data[sapply(data, is.null)] <- NA
      data <- as.data.frame(data)
      for(i in 1:ncol(data)) {
        data[,i] <- do.call(rbind, lapply(data[,i], rbind))[,1]
      }

    } else {
      a[sapply(a, is.null)] <- NA
      a$yield[sapply(a$yield, is.null)] <- NA
      a$projections$yield[sapply(a$projections$yield, is.null)] <- NA
      a$projections$harvestDate[is.null(a$projections$harvestDate)] <- NA

      data <- data.frame(planting_id = unlist(a$id), crop = unlist(a$crop), field_id = unlist(a$field),
                         plantingDate = unlist(a$plantingDate), yieldAmount = unlist(a$yield$amount), yieldUnits = unlist(a$yield$units),
                         actualHarvestDate = unlist(a$harvestDate), projectedYieldAmount = unlist(a$projections$yield$amount),
                         projectedYieldUnits = unlist(a$projections$yield$units), projectedHarvestDate = unlist(a$projections$harvestDate))
    }
  }

  if(!is.null(a$statusCode)) {
    stop(a$detailedMessage)
  } else {
    return(as.data.frame(data))
  }

}
