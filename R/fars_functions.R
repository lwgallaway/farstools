library(dplyr)
library(tidyr)
library(maps)
library(graphics)

#' Data import function
#'
#' This function is used to take data from the US-NHTSA Fatality Analysis Reporting System (FARS)
#' in a .csv file and return it back as a tidyverse dataframe(tibble).
#'
#' @param filename A character string with the location of the data file.
#' @return a tibble of FARS data for a given year
#' @importFrom dplyr tbl_df %>%
#' @importFrom readr read_csv
#' @note Function will also check is file exists and throw an error if it is not present.
#' @examples
#' \dontrun{
#' fars_read("accident_2014.csv.bz2")
#' }
#'
#'
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}


#'Make file name
#'
#'Function to make a custom filename based on a given year. Given an input year
#'(ex. \code{1983}) it will return a character string (\code{[1] "accident_1983.csv.bz2"})
#'
#'@param year Year as either character or numeric string
#'@return character string
#'@examples
#'\dontrun{
#'make_filename(1983)
#'}

make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}



#'FARS year/month parser
#'
#'Function to make a list where each list item is a tibble of month and year data.
#'Each month & year entry corresponds to a fatal crash. The \code{years} argument will take numeric vector.
#'However, the \code{years} argument is used to build filename paths, so all files must be stored
#'in the working directory or the working directory changed before running the function.
#'
#'@inheritParams  make_filename
#'@param years A numeric vector of calendar years
#'@return a list of data frames the length of the \code{years} argument
#'@importFrom dplyr mutate select
#'@note function will try to continue if it finds any invalid years and will throw a warning
#'@examples
#'\dontrun{
#'fars_read_years(2013:2015)
#'}

fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}


#'FARS summary data
#'
#'This function will process the FARS data to produce a tibble that shows the number of
#'fatalities by month per year
#'
#'@inheritParams fars_read_years
#'@return a tibble of incidents by month per year
#'@importFrom dplyr bind_rows group_by summarize
#'@importFrom tidyr spread
#'@examples
#'\dontrun{
#'fars_summarize_years(2013:2015)
#'}
#'@export

fars_summarize_years <- function(years){
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}



#'Mapping FARS data by state
#'
#'This function will map the fatalities for a given state using latitude and longitude.
#'
#'@param state.num 2-digit integer that determines which state will be plotted. See references for list of state codes.
#'@param year a four digit integer for a singular year
#'@references see US Census FIPS numbers for state codes \href{State Codes}{https://www.census.gov/geo/reference/ansi_statetables.html}
#'@return a plot showing the fatality locations in a particular state
#'@note Function will throw an error if \code{state num} is not a valid state. Function will also see
#'       if there are no accidents for a given state/year and pass a message to that effect.
#'@importFrom dplyr filter
#'@importFrom maps map
#'@importFrom graphics points
#'@examples
#'\dontrun{
#'fars_map_state(39, 2014)
#'}
#'@export

fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
