#' @title Get Models
#'
#' @description
#' \code{get_models} Calls Models endpoint of API
#'
#' @details
#' This API provides the list of available models in the aWhere platform.
#' Today a variety of crop growth stage models are generally available, with many more on the way.
#'
#' Each models applies to particular crops from the Crops API, and after an initial
#' review of the available models, you can often save the model ID and simply reference the details and results as needed.
#'
#' @references https://developer.awhere.com/api/reference/models/get-models
#'
#' @param - model_id: Either a model id to retrieve information for that specific model
#'                   or an empty string to retrieve information on all model associated
#'                   with the user's aWhere API account (string - optional)
#' @param - keyToUse: aWhere API key to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#' @param - secretToUse: aWhere API secret to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#' @param - tokenToUse: aWhere API token to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#'
#' @import httr
#'
#' @return - data.frame containing information about requested model(s)
#'
#' @examples
#' \dontrun{get_models('BarleyGenericMSU')
#'          get_models()
#' }

#' @export

get_models <- function(model_id = ''
                       ,keyToUse = awhereEnv75247$uid
                       ,secretToUse = awhereEnv75247$secret
                       ,tokenToUse = awhereEnv75247$token) {
  
  checkCredentials(keyToUse,secretToUse,tokenToUse)
  
  ## Create Request
  url <- "https://api.awhere.com/v2/agronomics/models/"
  
  if(model_id != "") {
    url <- paste0(url, model_id)
  }
  
  doWeatherGet <- TRUE
  while (doWeatherGet == TRUE) {
    request <- httr::GET(url,
                         httr::content_type('application/json'),
                         httr::add_headers(Authorization =
                                             paste0("Bearer ", tokenToUse)))
    
    a <- suppressMessages(httr::content(request))
    
    doWeatherGet <- check_JSON(a,request)[[1]]
  }
  
  ## Create & fill data frame
  if(model_id == "") {
    
    data <- as.data.frame(do.call(rbind, lapply(a$models, rbind)))[, c(1:5)]
    source <- as.data.frame(do.call(rbind, lapply(data$source, rbind)))
    data$source <- NULL
    data <- cbind(data, source)
    
    colnames(data) <- c("modelId", "name", "description", "type", "sourceName", "sourceLink")
    rownames(data) <- c(1:nrow(data))
    
    data <- data.frame(lapply(data, as.character), stringsAsFactors=FALSE)
    
    data <- as.matrix(data)
    data[sapply(data, is.null)] <- NA
    data <- as.data.frame(data)
    
  } else {
    source <- as.data.frame(do.call(cbind, lapply(a$source, cbind)))
    data <- as.data.frame(rbind(a))[,c(1:4)]
    data <- cbind(data, source)
    
    colnames(data) <- c("modelId", "name", "description", "type", "sourceName", "sourceLink")
    rownames(data) <- c(1:nrow(data))
  }
  
  data <- dplyr::mutate_if(data, is.factor, as.character)
  
  if(nrow(data) == 0) {
    stop(a$simpleMessage)
  } else {
    return(as.data.frame(data))
  }
}







#' @title Get Model Details
#'
#' @description
#' \code{get_model_details} Calls Model Details endpoint of API
#'
#' @details
#' Every model has its own parameters and criteria it uses to calculate the results.
#' This API will return those details; for the growth stage models currently available
#' this API returns information about the crop stages that the model may return.
#'
#' @references https://developer.awhere.com/api/reference/models/details
#'
#' @param - model_id: A model id to retrieve details for that specific model
#' @param - keyToUse: aWhere API key to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#' @param - secretToUse: aWhere API secret to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#' @param - tokenToUse: aWhere API token to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#'
#' @import httr
#'
#' @return - data.frame containing details about requested model
#'
#' @examples
#' \dontrun{get_model_details('BarleyGenericMSU')
#' }

#' @export

get_model_details <- function(model_id
                              ,keyToUse = awhereEnv75247$uid
                              ,secretToUse = awhereEnv75247$secret
                              ,tokenToUse = awhereEnv75247$token) {
  
  checkCredentials(keyToUse,secretToUse,tokenToUse)
  
  ## Create Request
  url <- "https://api.awhere.com/v2/agronomics/models/"
  
  url <- paste0(url, model_id, "/details")
  
  
  doWeatherGet <- TRUE
  while (doWeatherGet == TRUE) {
    request <- httr::GET(url,
                         httr::content_type('application/json'),
                         httr::add_headers(Authorization =
                                             paste0("Bearer ", tokenToUse)))
    
    a <- suppressMessages(httr::content(request))
    
    doWeatherGet <- check_JSON(a,request)[[1]]
  }
  
  
  a[sapply(a, is.null)] <- NA
  
  data <- data.frame(biofix = a$biofix, gddMethod = as.character(a$gddMethod), gddBaseTemp = a$gddBaseTemp, gddMaxBoundary = as.integer(a$gddMaxBoundary),
                     gddMinBoundary = as.integer(a$gddMinBoundary), gddUnits = as.character(a$gddUnits))
  
  stages <- as.data.frame(do.call(rbind, lapply(a$stages, rbind)))[, c(2:5)]
  stages <- data.frame(lapply(stages, as.character), stringsAsFactors=FALSE)
  stages$gddThreshold <- as.integer(stages$gddThreshold)
  
  data <- cbind(data, stages)
  colnames(data) <- c("biofix", "gddMethod", "gddBaseTemp", "gddMaxBoundary", "gddMinBoundary",
                      "gddUnits", "id", "stage", "description", "gddThreshold")
  rownames(data) <- c(1:nrow(data))
  
  data <- dplyr::mutate_if(data, is.factor, as.character)
  
  
  if(nrow(data) == 0) {
    stop(a$simpleMessage)
  } else {
    return(as.data.frame(data))
  }
}






#' @title Get Model Results
#'
#' @description
#' \code{get_model_details} Calls Model Results endpoint of API
#'
#' @details
#' Running a model is as simple as requesting its current results. The aWhere platform
#' will use the information previously provided about a field and planting to generate
#' the relevant, field-specific information. For growth stage models, the results are
#' the current stage as well as information about the most recent and next stage.
#' 
#' Important: Models require the use of the Fields and Plantings APIs to store important
#' information about a field and crop. When a Field is specified for this API, it will
#' select the most recently created Planting record to retrieve the crop and planting date.
#'
#' @references https://developer.awhere.com/api/reference/models/results
#'
#' @param - model_id: A model id to retrieve details for that specific model
#' @param - keyToUse: aWhere API key to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#' @param - secretToUse: aWhere API secret to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#' @param - tokenToUse: aWhere API token to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#'
#' @import httr
#'
#' @return - data.frame containing details about requested model
#'
#' @examples
#' \dontrun{get_model_results('Field1', 'BarleyGenericMSU')
#' }

#' @export

get_model_results <- function(field_id
                              ,model_id
                              ,keyToUse = awhereEnv75247$uid
                              ,secretToUse = awhereEnv75247$secret
                              ,tokenToUse = awhereEnv75247$token) {
  
  checkCredentials(keyToUse,secretToUse,tokenToUse)
  
  ## Create Request
  url <- "https://api.awhere.com/v2/agronomics/fields/"
  
  url <- paste0(url, field_id, "/models/", model_id, "/results")
  
  
  doWeatherGet <- TRUE
  while (doWeatherGet == TRUE) {
    request <- httr::GET(url,
                         httr::content_type('application/json'),
                         httr::add_headers(Authorization =
                                             paste0("Bearer ", tokenToUse)))
    
    a <- suppressMessages(httr::content(request))
    
    doWeatherGet <- check_JSON(a,request)[[1]]
  }
  
  
  a[sapply(a, is.null)] <- NA
  
  data <- data.frame(biofixDate = a$biofixDate, gddUnits = a$gddUnits, modelId = a$modelId, latitude = a$location$latitude,
                     longitude = a$location$longitude, fieldId = a$location$fieldId, plantingDate = a$plantingDate)
  
  previousStages <- data.frame()
  if(class(a$previousStages) == "list" & length(a$previousStages) > 0) {
    previousStages <- as.data.frame(do.call(rbind, lapply(a$previousStages, rbind)))[, c(1:5)]
    previousStages$stageType <- "previous"
    previousStages$accumulatedGdds <- NA
    previousStages$gddRemaining <- NA
  }
  
  currentStage <- data.frame()
  if(class(a$currentStage) == "list" & length(a$currentStage) > 0) {
    currentStage <- as.data.frame(rbind(a$currentStage))
    currentStage$stageType <- "current"
    currentStage$gddRemaining <- NA
  }
  
  nextStage <- data.frame()
  if(class(a$nextStage) == "list" & length(a$nextStage) > 0) {
    nextStage <- as.data.frame(rbind(a$nextStage))
    nextStage$date <- NA
    nextStage$stageType <- "next"
    nextStage$accumulatedGdds <- NA
  }
  
  stages <- rbind(previousStages, currentStage, nextStage)
  stages <- data.frame(lapply(stages, as.character), stringsAsFactors=FALSE)
  stages <- stages[, c("date", "id", "stage", "description", "gddThreshold", "stageType", "accumulatedGdds", "gddRemaining")]
  
  data <- cbind(data, stages)
  colnames(data) <- c("biofixDate", "gddUnits", "modelId", "latitude", "longitude", "fieldId", "plantingDate",
                      "date", "id", "stage", "description", "gddThreshold", "stageType", "accumulatedGdds", "gddRemaining")
  rownames(data) <- c(1:nrow(data))
  
  data <- dplyr::mutate_if(data, is.factor, as.character)
  
  data <- suppressWarnings(dplyr::mutate_at(data, c("biofixDate", "plantingDate", "date"), as.Date))
  data <- suppressWarnings(dplyr::mutate_at(data, c("gddThreshold", "accumulatedGdds", "gddRemaining"), as.numeric))
  data <- dplyr::mutate_at(data, c("accumulatedGdds", "gddRemaining"), round, 2)
  
  if(nrow(data) == 0) {
    stop(a$simpleMessage)
  } else {
    return(as.data.frame(data))
  }
}
