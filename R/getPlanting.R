#' @title GetPlanting
#'
#' @description
#' \code{GetPlanting} Gets a planting or list of plantings in the aWhere platform for which you can request weather
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
#' @param - fieldId: a field ID to look within (string)
#' @param - plantingId: a planting ID to look for (string)
#' @param - current: whether to just get current plantings(T) or include historical plantings(F).
#'                   To get most recent planting record for a field, set current to TRUE and do not pass in a plantingId (boolean)
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

GetPlanting <- function(fieldId = "", plantingId = "", current = F) {


  ## Create Request
  url <- "https://api.awhere.com/v2/agronomics/"

  if(fieldId != "") {
    url <- paste0(url, "fields/", fieldId, "/plantings")
  } else {
    url <- paste0(url, "plantings/")
  }

  if(plantingId != "") {
    url <- paste0(url, plantingId)
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
      GetAccessToken(awhereEnv75247$uid,awhereEnv75247$secret)
    } else {
      doWeatherGet <- FALSE
    }
  }

  ## Get data

  ## Create & fill data frame
  if(is.null(a$statusCode)) {
    if(plantingId == "") {
      data <- as.data.frame(do.call(rbind, lapply(a$plantings, rbind)))[, c(1:7)]
      data <- cbind(data, do.call(rbind, lapply(data$yield, rbind)))
      data$yield <- NULL
      data <- cbind(data, do.call(rbind, lapply(data$projections, rbind)))
      data <- cbind(data, do.call(rbind, lapply(data$yield, rbind)))
      data$yield <- NULL
      data$projections <- NULL

      colnames(data) <- c("plantingId", "crop", "fieldId", "plantingDate", "actualHarvestDate", "yieldAmount", "yieldUnits",
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

      data <- data.frame(plantingId = unlist(a$id), crop = unlist(a$crop), fieldId = unlist(a$field),
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
