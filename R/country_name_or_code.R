#' Convert between country codes and country names
#'
#' This function converts between 2-letter country codes and country names. If the
#' input contains all 2-letter codes, the function returns the corresponding country
#' names. If the input contains all country names, the function returns the
#' corresponding 2-letter codes.
#'
#' @param x A character vector containing either 2-letter country codes or country
#' names.
#' @return A character vector of the same length as \code{x}, containing either
#' country names or 2-letter country codes, depending on the input.
#' @examples
#' # Convert country codes to country names
#' country_name_or_code(c("US", "GB", "CA"))
#'
#' # Convert country names to country codes
#' country_name_or_code(c("United States", "United Kingdom", "Canada"))
#'
#' # Error: input contains both country codes and country names
#' country_name_or_code(c("US", "GB", "CA"))
#'
#' @importFrom countrycode
#' @export
country_name_or_code <- function(x) {
  if (all(is.na(x) | nchar(x) == 2)) {
    countrycode::countrycode(x, "iso2c", "country.name")
  } else if (all(!is.na(x) & nchar(x) > 2)) {
    countrycode::countrycode(x, "country.name", "iso2c")
  } else {
    stop("Input must contain either all 2-letter country codes or all country names")
  }
}
