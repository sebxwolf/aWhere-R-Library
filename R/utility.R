`:=` <- data.table::`:=`
#' @importFrom lubridate ymd
ymd <- lubridate::ymd


is.leapyear = function(year){
  #http://en.wikipedia.org/wiki/Leap_year
  return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
}