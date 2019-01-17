
#' @title Removes unnecessary columns from API Return
#'
#' @description
#' \code{removeUnnecessaryColumns} Removes unnecessary columns from API Return
#'
#' @param - ata: data.table to have columns removed from

removeUnnecessaryColumns <- function(data) {

  varNames <- colnames(data)

  #This removes the non-data info returned with the JSON object
  suppressWarnings(data[,grep('_links',varNames) := NULL])
  suppressWarnings(data[,grep('.units',varNames) := NULL])
  suppressWarnings(data[,grep('latitude',varNames) := NULL])
  suppressWarnings(data[,grep('longitude',varNames) := NULL])
  suppressWarnings(data[,grep('fieldId',varNames) := NULL])
  suppressWarnings(data[,c('soilTemperatures','soilMoisture') := NULL])

  return(data)
}
