#' Filter OAB Data by Months and Years
#'
#' This function filters OAB data based on specified months and year using
#' the FECHA_FIN_MAREA column from the trips dataframe, then filters all
#' related dataframes to match only the trips that passed the filter.
#'
#' @param oab_data A list containing OAB dataframes. This list is the result
#'   of importOABFiles() function from sapmuebase package. Include a 'trips'
#'   data frame with a FECHA_FIN_MAREA column in "dd/mm/yyyy" format. All other
#'   dataframes in the list must have a COD_MAREA column for filtering.
#'   Typically includes: trips, hauls, catches, lengths, litter, and accidentals.
#' @param months Numeric vector of months to filter (1-12). Can be a single
#'   month or multiple months. Required parameter.
#' @param year Numeric value representing the year to filter (e.g., 2025).
#'   Must be a single year. Required parameter.
#'
#' @return A list with the same structure as the input oab_data, containing
#'   filtered dataframes. All dataframes are filtered to include only records
#'   whose COD_MAREA exists in the filtered trips dataframe. The trips dataframe
#'   is filtered by the specified months and year.
#'   If an error occurs during filtering, the function stops execution with
#'   an error message.
#'
#' @details
#' The function first filters the trips dataframe by converting the
#' FECHA_FIN_MAREA column to POSIXlt format and extracting the month (1-12)
#' and year components. It extracts the COD_MAREA values from the filtered
#' trips, then uses lapply to filter all dataframes in the oab_data list,
#' keeping only records whose COD_MAREA exists in the filtered trips.
#'
#' @note
#' The function assumes that:
#' \itemize{
#'   \item The oab_data list contains a 'trips' element
#'   \item The trips dataframe has a FECHA_FIN_MAREA column in "dd/mm/yyyy" format
#'   \item All dataframes in the list have a COD_MAREA column for linking records
#'   \item The year parameter is a single numeric value, not a vector
#' }
#'
#' @export
filter_by_months <- function(oab_data,
                             months,
                             year) {

  if (!"FECHA_FIN_MAREA" %in% colnames(oab_data$trips)) {
    stop("The trips dataframe must contain a 'FECHA_FIN_MAREA' column")
  }

  tryCatch(
    {

      filtered_trips <- oab_data$trips[(as.POSIXlt(oab_data$trips$FECHA_FIN_MAREA, format = "%d/%m/%Y")$mon + 1) %in% months &
                                         (as.POSIXlt(oab_data$trips$FECHA_FIN_MAREA, format = "%d/%m/%Y")$year + 1900) == year,
                                       "COD_MAREA"]

      filtered_data <- lapply(oab_data, function(x){
        x <- x[x$COD_MAREA %in% filtered_trips, ]
        return(x)
      })

    return(filtered_data)
    },

    warning = function(w) {
      message("A warning occurred while filtering by months: ", w)
    },

    error = function(e) {
      stop("An error occurred while filtering by months: ", e$message)
    }
  )
}
