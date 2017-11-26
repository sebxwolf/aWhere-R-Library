#' @title Get Access Token
#'
#' @description
#' \code{get_token} gets Access Token for V2 aWhere API
#'
#' @details
#' This script provides an aWhere access token for the current session of the API. Information for the key and secret in this 
#' function can be found on a user's account at developer.awhere.com, under the apps.
#'
#' @param - uid: Consumer key associated with the user's aWhere API account
#' @param - secret: Consumer secret associated the user's aWhere API account
#' @param - use_enviroment: Optional logical value, determines whether API access
#' token will be saved in a local locked environment in addition to being returned
#' by the function. Defaults to \code{TRUE} to avoid breaking existing code.
#' 
#' @return List with three elements:#' 
#' error: logical indicating whether there was an error
#' error_message: \code{NULL} if error is \code{FALSE}, a character error message otherwise
#' token: aWhere API access token value
#'
#' @examples
#' \dontrun{get_token("uid", "secret")}
#' \dontrun{token_response <- get_token('uid', 'secret', use_environment = FALSE)}
#' @export

get_token <- function(uid, secret, use_environment = TRUE) {

  url <- "https://api.awhere.com/oauth/token"

  authen_char <- charToRaw(paste0(uid,':',secret))

  request <- httr::POST(url, body='grant_type=client_credentials',
                        httr::content_type('application/x-www-form-urlencoded'),
                        httr::add_headers(Authorization = paste0('Basic ', base64enc::base64encode(authen_char))))

  a <- suppressMessages(httr::content(request, as = "text"))

  if (request$status_code != 200) {
    error <- TRUE
    error_message <- 'The UID/Secret combination is incorrect.'
    token <- NULL
    
    cat(paste(error_message, '\n'))
  } else {
    
    error <- FALSE
    error_message <- NULL
  
    parsedResponse <- jsonlite::fromJSON(a, simplifyDataFrame = FALSE)

    # Keeping environment approach for backward compatibility,
    # but adding an optional argument to skip it.
    if (use_environment) {
      if (exists('awhereEnv75247') == FALSE) {
        awhereEnv75247 <- new.env()
        assign('awhereEnv75247',awhereEnv75247,envir = baseenv())
        rm(awhereEnv75247)
      }
    
      if (exists('uid',envir = awhereEnv75247,inherits = FALSE) == TRUE) {
        if (bindingIsLocked('uid',awhereEnv75247) == TRUE) {
          unlockBinding('uid',awhereEnv75247)
        }
      }
      if (exists('secret',where = awhereEnv75247,inherits = FALSE) == TRUE) {
        if (bindingIsLocked('secret',awhereEnv75247) == TRUE) {
          unlockBinding('secret',awhereEnv75247)
        }
      }
      if (exists('token',where = awhereEnv75247,inherits = FALSE) == TRUE) {
        if (bindingIsLocked('token',awhereEnv75247) == TRUE) {
          unlockBinding('token',awhereEnv75247)
        }
      }
    
      awhereEnv75247$uid    <- uid
      awhereEnv75247$secret <- secret
      awhereEnv75247$token  <- token <- parsedResponse$access_token
    
      lockBinding('uid',    awhereEnv75247)
      lockBinding('secret', awhereEnv75247)
      lockBinding('token',  awhereEnv75247)
    
      lockEnvironment(awhereEnv75247,bindings = TRUE)
      rm(awhereEnv75247)
    }
  }
  
  list(
    error = error,
    error_message = error_message,
    token = token)
}

#' @title Load Credentials.
#'
#' @description
#' \code{load_credentials} loads credential information from text file to use in
#'  calls to aWhere API
#'
#' @details
#' This script creates loads a valid aWhere API token into memory to be used in making API calls.
#' Instead of typing in the password and username manually and calling the get_token(uid,secret) function,
#' you can store this information in a txt file in some arbitrary directory.
#' The first line of the text file should be the provided uid/username, the
#' second line should be the associated secret.  A blank 3rd line should be inserted to prevent
#' an error from being returned by R
#'
#' @param - path_to_credentials: absolute or relative path to the text file
#' @return vector with uid and secret in positions 1, 2
#'
#' @examples
#' \dontrun{load_credentials("C:/aWhere/credentials/credentials.txt")}
#'
#' @export

load_credentials <- function(path_to_credentials) {
  credentials <- readLines(path_to_credentials)

  uid <- credentials[1]
  secret <- credentials[2]

  get_token(uid, secret)
}

