#' @title Plan Logic of API Calls
#'
#' @description
#' \code{plan_APICalls} Calculate logic of API calls
#'
#' @details
#' Calculate logic of API calls based on the number of days that can be returned
#' with each call as well as start and end dates
#'
#' @param - JSON object returned from aWhere API
#' @return list(allDates,loops)

plan_APICalls <- function(day_start
                          ,day_end
                          ,numObsReturned) {

  numOfDays <- as.numeric(difftime(lubridate::ymd(day_end)
                                   ,lubridate::ymd(day_start)
                                   , units = 'days'))
  allDates <- seq(as.Date(lubridate::ymd(day_start))
                  ,as.Date(lubridate::ymd(day_end))
                  , by="days")

  loops <- ((length(allDates))) %/% numObsReturned
  remainder <- ((length(allDates))) %% numObsReturned


  if(remainder > 0) {
    loops <- loops + 1
  }


  return(list(allDates,loops))
}