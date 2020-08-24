#' Get Zendesk Tickets
#'
#' This function takes your Email Id, authentication token,
#' sub-domain and start time as parameters and gets all the
#' tickets which have been updated on or after the start
#' time parameter. By default each page returns 1000 unique
#' tickets and an "after_cursor" value which stores a
#' pointer to the next page. After getting the first page
#' it uses the pointer to fetch the subsequent pages.
#'
#' The start time parameter should be in 'UTC' format as
#' Zendesk uses the 'UTC' time zone when retrieving tickets
#' after the start time. For example, the US Eastern Time Zone
#' is currently four hours being UTC. If one wanted to get tickets
#' starting on August 1 at 12 am, you would need to enter
#' "2020-08-01 04:00:00". The user must do proper adjustment
#' to accommodate the time zone difference, if desired. A
#' date can be provided, it will retrieve results as of 12 am
#' in the UTC time zone.
#'
#' Its not a good practice to write down these authentication
#' parameters in your code. There are various methods and
#' packages available that are more secure; this package
#' doesn't require you to use any one in particular.
#'
#' @references \url{https://developer.zendesk.com/rest_api
#' /docs/support/incremental_export#start_time}
#'
#' @param email_id Zendesk Email Id (username).
#' @param token Zendesk API token.
#' @param start_time Date or Datetime object to get all
#' tickets modified after that date. Optional - by default
#' all non-archived tickets will be returned.
#' @param subdomain Your organization's Zendesk sub-domain.
#'
#' @return a Data Frame containing all tickets after the
#' start time.
#'
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom jsonlite "fromJSON"
#' @importFrom httr "content"
#' @importFrom tidyr "pivot_wider"
#' @importFrom purrr "map_dfr"
#' @importFrom plyr "rbind.fill"
#'
#' @export
#'
#' @examples \dontrun{
#' all_tickets <- get_tickets(email_id, token, subdomain, start_time = "2020-08-01 04:00:00")
#' }


get_tickets <- function(email_id, token, subdomain, start_time = 0){

  user <- paste0(email_id, "/token")
  pwd <- token
  subdomain <- subdomain
  url <- paste0("https://", subdomain,
                ".zendesk.com/api/v2/incremental/tickets/cursor.json?start_time=",
                to_unixtime(start_time))

  cursor_url <- paste0("https://",
                       subdomain,
                       ".zendesk.com/api/v2/incremental/tickets/cursor.json?cursor=")

  # Get the cursor value.
  after_cursor <- NULL
  while(is.null(after_cursor)){
    request_ticket<-  httr::GET(url = url,httr::authenticate(user, pwd))
    if(!is.null((jsonlite::fromJSON(httr::content(request_ticket, 'text')))$after_cursor)){
      after_cursor <- (jsonlite::fromJSON(httr::content(request_ticket, 'text')))$after_cursor
    }
  }
  ticket_content1 <- httr::content(request_ticket, 'text')
  content_json1 <- jsonlite::fromJSON(ticket_content1, flatten = TRUE)
  ticket_df1 <- as.data.frame(content_json1$tickets)


  # Now we have to use the cursor to paginate it through other pages.
  request_ticket_cur <- list()

  i <- 1
  stop_paging <- FALSE
  while(stop_paging == FALSE){
    request_ticket_cur[[i]] <- httr::RETRY('GET',
                                           url = paste0(cursor_url, after_cursor), 
                                           httr::authenticate(user, pwd),
                                           times = 4,
                                           pause_min = 10,
                                           terminate_on = NULL,
                                           terminate_on_success = TRUE,
                                           pause_cap = 5)
    after_cursor <- (jsonlite::fromJSON(httr::content(request_ticket_cur[[i]], 'text'),flatten = TRUE))$after_cursor
    if(isTRUE((jsonlite::fromJSON(httr::content(request_ticket_cur[[i]], 'text'), flatten = TRUE))$end_of_stream)){
      stop_paging <- TRUE
    }
    i <- i+1
  }


  build_data_frame <- function(c){
    tickets <- as.data.frame((jsonlite::fromJSON(httr::content(request_ticket_cur[[c]], 'text'), flatten = TRUE))$tickets)
  }

  ticket_df2 <- purrr::map_dfr(1:length(request_ticket_cur), build_data_frame)

  # Merging both the data frames obtained using Cursor and Time based incremental export.
  ticket_merged <- plyr::rbind.fill(ticket_df1, ticket_df2)

  # There is a column called "custom fields" inside the tickets data frame.
  # It is specific to each sub-domain(organization).
  # It is a data frame so we need to extract it and add them as column headers.

  pivot_data_frame <- function(c){
   pivot_df <- as.data.frame(ticket_merged$custom_fields[c])%>%
     tidyr::pivot_wider(names_from= .data$id, values_from= .data$value)
  }


 ticket_final <- purrr::map_dfr(1:nrow(ticket_merged), pivot_data_frame)
 ticket_final <- bind_cols(ticket_merged, ticket_final)

 return(ticket_final)

}
