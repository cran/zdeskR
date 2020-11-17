#' Returns all available zendesk users.
#'
#' It takes your Email Id, authentication token,
#' sub-domain and parse all the users in a list.
#' It iterates through all the pages returning only 100 users per
#' page until the "next_page" parameter becomes null indicating
#' there are no more pages to fetch.
#'
#' It's not a good practice to write down these authentication
#' parameters in your code. There are various methods and
#' packages available that are more secure; this package
#' doesn't require you to use any one in particular.
#'
#'
#' @references \url{https://developer.zendesk.com/rest_api
#' /docs/support/users}
#'
#' @param email_id Zendesk Email Id (username).
#' @param token Zendesk API token.
#' @param subdomain Your organization's Zendesk sub-domain.
#'
#' @return Data Frame with user details
#'
#' @import dplyr
#' @importFrom jsonlite "fromJSON"
#' @importFrom httr "content"
#' @importFrom httr "authenticate"
#' @importFrom purrr "map_dfr"
#'
#' @export
#'
#' @examples \dontrun{
#' users <- get_users(email_id, token, subdomain)
#' }

get_users <- function(email_id, token, subdomain) {

  user <- paste0(email_id, "/token")
  pwd <- token
  subdomain <- subdomain
  url_users <- paste0("https://", subdomain, ".zendesk.com/api/v2/users.json?role=end-user&page=")

  #Stop Pagination when the parameter "next_page" is null.
  req_users <- list()
  stop_paging <- FALSE
  i <- 1
  while(stop_paging == FALSE){
    req_users[[i]] <-  httr::RETRY("GET",
                                     url = paste0(url_users, i),
                                     httr::authenticate(user, pwd),
                                     times = 4,
                                     pause_min = 10,
                                     terminate_on = NULL,
                                     terminate_on_success = TRUE,
                                     pause_cap = 5)
    if(is.null((jsonlite::fromJSON(httr::content(req_users[[i]], 'text')))$next_page)){
      stop_paging <- TRUE
    }
    i <- i + 1
  }

  build_data_frame <- function(c){
    users <- as.data.frame((jsonlite::fromJSON(httr::content(req_users[[c]], "text"), flatten = TRUE))$users)
  }

  users_df <- purrr::map_dfr(1:length(req_users), build_data_frame)

  return(users_df)
}
