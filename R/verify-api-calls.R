
#' @title Verify User Wants Make API Calls
#'
#' @description
#' \code{verify_api_calls} Tells user how many api calls will be made to pull the data requested
#'
#' @param - grid: data frame returned from create_awhere_grid

verify_api_calls <- function(grid) {
  cat(paste0('This query will require ',nrow(grid),' API Calls \n'))
  makeAPICalls <- readline("\n Do you wish to proceed? Type yes to begin API calls: ")

  if (tolower(makeAPICalls) != 'yes') {
    stop('User Input indicated they did not want to proceed with making API Calls')
  }
}
