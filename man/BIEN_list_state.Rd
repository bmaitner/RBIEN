% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BIEN.R
\name{BIEN_list_state}
\alias{BIEN_list_state}
\title{Extract a species list by state/province}
\usage{
BIEN_list_state(
  country = NULL,
  country.code = NULL,
  state = NULL,
  state.code = NULL,
  cultivated = FALSE,
  new.world = NULL,
  ...
)
}
\arguments{
\item{country}{A single country or a vector of countries equal in length to the vector of states/provinces.}

\item{country.code}{A single country code or a vector of country codes equal in length to the vector of states/province codes.}

\item{state}{A state or vector of states (or other primary political divisions, e.g. provinces).}

\item{state.code}{A single state/province code, or a vector of states/province codes.}

\item{cultivated}{Return information on cultivation status?  Default is FALSE.}

\item{new.world}{NULL (The default) returns global records, TRUE returns only New World, and FALSE only Old World.}

\item{...}{Additional arguments passed to internal functions.}
}
\value{
Dataframe containing species list(s) for the specified states/provinces.
}
\description{
BIEN_list_state produces a list of all species with geovalidated occurrences falling within specified state(s) or province(s).
}
\note{
Political division (or political division code) spelling needs to be exact and case-sensitive, see \code{\link{BIEN_metadata_list_political_names}} for a list of political divisions and associated codes.
}
\examples{
\dontrun{
BIEN_list_state("United States","Michigan")
state_vector<-c("Michigan","Arizona")
BIEN_list_state(country="United States", state= state_vector)}
}
\seealso{
Other list functions: 
\code{\link{BIEN_list_all}()},
\code{\link{BIEN_list_country}()},
\code{\link{BIEN_list_county}()},
\code{\link{BIEN_list_sf}()}
}
\concept{list functions}
