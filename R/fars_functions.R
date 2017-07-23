#'
#' fars_read function
#'
#' This function takes as "input" the name of the file and return a
#' dataframe using the readr::read_csv function.
#'
#' @param filename = a character as the file name of the data in a csv format
#' @import readr::read_csv
#' @import dplyr::tbl_df
#' @return The fars_read function will return a dataframe
#' @note In case that the file does not exist will return the message indicating so.
#' You will need to save the data files in the working directory to avoid error.
#' @examples
#'\dontrun{
#'   fars_read("accident_2013d.csv.bz2")
#'   }
#'@export
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

#'
#' make_filename function
#'
#' This function create a file name based on a given "year" that is part of the file name.
#'
#' @param year = Is the year (integer) of the data that is part of the file name like "accident_2013d.csv.bz2"
#' @return The make_filename function returns the name of the file with the "year" incorporated in the name of the file,
#' like year=2013 to "accident_2013d.csv.bz2".
#' @note You will need to save the data files in the working directory in order to avoid error.
#' @examples
#'\dontrun{
#'  >example1<-make_filename(2013)
#'  >example1
#'  >accident_2013d.csv.bz2
#'  }
#' @export
#'
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}


#'
#' fars_read_years function
#'
#' This function takes a vector of years as input, and them select the year and month information contained in
#' the files (like "accident_YYYYd.csv.bz2") saved in the working directory.
#' The files will be the ones where the name includes one of the years provided, after appliying
#' the "make_filename" function.
#'
#' @param years = is a vector containing integer values of years to be evaluated like c(2013,2014,2015)
#' @return The fars_read_years returns a data frame with the month and years available in the data
#' @import dplyr::mutate
#' @import dplyr::select
#' @import magrittr
#' @note A warning message will be given in the case that the file or the year and month in the file
#' are not available.
#' @examples
#' \dontrun{
#'
#' fars_read_years(c(2013,2014,2015))
#'
#' }
#' @export
#'
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


#'
#' fars_summarize_years function
#'
#'This function takes a vector of years, select and summarize the observations by year and month contained
#'in the files "accident_YYYYd.csv.bz2". The function will provide the warnings and messages included in the
#'fars_read and fars_read_years functions.
#'
#'
#' @param years = Is a vector containing the years to be evaluated, like c(2013,2014,2015)
#' @return The function fars_summarize_years returns a data frame with the summarized numbers of month and years
#' available in the data
#' @import dplyr::bind_rows
#' @import dplyr::group_by
#' @import dplyr::summarize
#' @import tidyr::spread
#' @import magrittr::%>%
#' @note In case that the file does not exist will return the message indicating so.
#' You will need to save the data files in the working directory.
#' @examples
#' \dontrun{
#'
#' fars_summarize_years (c(2013,2014,2015))
#'
#' }
#' @export
#'
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#'
#' fars_map_state function
#'
#' This function takes a year and state numbers and create a graph with the Highway Traffic
#' Fatality located by longitud and latitude.
#' The function will provide the warnings and messages included in the fars_read and make_file.
#'
#' @param year = an integer, the value of the year to be evaluated, example :c(2013,2014,2015)
#' @param state.num = an integer, indicator of the states to be evaluated, example :c(11,24,36)

#' @return This function returns a graph based on the Highway Traffic Fatality data, based in
#' the years and states selected, the Latitude and Longitud included in the data.
#' @import dplyr::filter
#' @import maps::map
#' @import graphics::points
#' @examples
#'\dontrun{
#'
#'fars_map_state(c(20,33,48), c(2013,2014,2015))
#'
#'}
#' @export
#'
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
