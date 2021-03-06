% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_custom_fields.R
\name{get_custom_fields}
\alias{get_custom_fields}
\title{Returns the system and all the custom fields defined by
your organization's zendesk administrator}
\usage{
get_custom_fields(email_id, token, subdomain)
}
\arguments{
\item{email_id}{Zendesk Email Id (username).}

\item{token}{Zendesk API token.}

\item{subdomain}{Your organization's Zendesk sub-domain.}
}
\value{
A data frame containing all ticket fields
}
\description{
It takes your Email Id, authentication token,
sub-domain  as parameters and gets the system and all
the custom fields available for a zendesk ticket.
}
\details{
It's not a good practice to write down these authentication
parameters in your code. There are various methods and
packages available that are more secure; this package
doesn't require you to use any one in particular.
}
\examples{
\dontrun{
fields <- get_custom_fields(email_id, token, subdomain)
}
}
\references{
\url{https://developer.zendesk.com/rest_api
/docs/support/ticket_fields}
}
