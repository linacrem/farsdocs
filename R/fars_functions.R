#' Read in a fars file
#'
#' @param filename The name of the file containing fars data
#'
#' @return A data.frame containing fars data
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples
#' \dontrun{fars_read("accident_2015.csv.bz2")}
#' \dontrun{fars_read("c:/workspace/R/accident_2015.csv.bz2")}
#'
#' @note The filename must exist or it will throw an error. Also, the file
#' must be able to be converted to a data.frame
#'
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Create a fars filename
#'
#' @param year Integer specifying the year of interest
#'
#' @return A properly formatted fars filename for the given year
#'
#' @examples
#' make_filename(2015)
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Read in one or more years of fars data
#'
#' @param years Vector of one or more years of interest
#'
#' @return List of data.frames, each row containing the months and year
#'
#' @importFrom dplyr mutate select
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{fars_read_years(2015)}
#' \dontrun{fars_read_years(c(2013, 2014, 2015))}
#'
#' @export
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

#' Summarize the number of fatal accidents by month and year
#'
#' @param years Vector of one or more years of interest
#'
#' @return a data.frame summarizing the number of fatal accidents by month and year
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{fars_summarize_years(2015)}
#' \dontrun{fars_summarize_years(c(2013, 2014, 2015))}
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>% 
                dplyr::group_by(year, MONTH) %>% 
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Display map of fatalities
#' 
#' @param state.num Integer state number as defined by 
#'     \href{https://www.census.gov/geo/reference/ansi_statetables.html}{the US Census Bureau}
#' @param year Integer specifying the year of interest
#'
#' @return A plot of all fatal accidents for the given state during the given year
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#' \dontrun{fars_map_state(26, 2015)}
#'
#' @note Using a state number that does not exist will throw an error. Also,
#' only the contiguous 48 states and District of Columbia can be mapped 
#'
#' @export
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