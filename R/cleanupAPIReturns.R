
#' @title Removes unnecessary columns from API Return
#'
#' @description
#' \code{removeUnnecessaryColumns} Removes unnecessary columns from API Return
#'
#' @param - data: data.table to have columns removed from

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



#' @title Removes unnecessary columns from API Return
#'
#' @description
#' \code{recalculateAccumulations} manually recalculate accumulations over API calls
#'
#' @param - dataList: list of data.table to have accumulations corrected

recalculateAccumulations <- function(dataList) {
  accumulatedColumns <- grep(pattern = 'accumulated'
                             ,x = colnames(dataList[[1]])
                             ,value = TRUE
                             ,fixed = TRUE)
  
  accumulatedColumns <- grep(pattern = '.units'
                             ,x = accumulatedColumns
                             ,invert = TRUE
                             ,value = TRUE
                             ,fixed = TRUE)
  
  for (x in 1:length(dataList)) {
    if (x > 1) {
      for (y in 1:length(accumulatedColumns)) {
        eval(parse(text = paste0('dataList[[x]][,',accumulatedColumns[y],' := ',accumulatedColumns[y],' + lastValue.accumulatedColumns$',accumulatedColumns[y],']')))
      }  
    }
    
    lastValue.accumulatedColumns <- dataList[[x]][.N,accumulatedColumns,with = FALSE]
  }
  
  return(dataList)
}
