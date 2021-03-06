% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_map_state}
\alias{fars_map_state}
\title{Mapping FARS data by state}
\usage{
fars_map_state(state.num, year)
}
\arguments{
\item{state.num}{2-digit integer that determines which state will be plotted. See references for list of state codes.}

\item{year}{a four digit integer for a singular year}
}
\value{
a plot showing the fatality locations in a particular state
}
\description{
This function will map the fatalities for a given state using latitude and longitude.
}
\note{
Function will throw an error if \code{state num} is not a valid state. Function will also see
      if there are no accidents for a given state/year and pass a message to that effect.
}
\examples{
\dontrun{
fars_map_state(39, 2014)
}
}
\references{
see US Census FIPS numbers for state codes \href{State Codes}{https://www.census.gov/geo/reference/ansi_statetables.html}
}
