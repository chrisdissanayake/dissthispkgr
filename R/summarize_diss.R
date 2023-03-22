#' Summarize data frame with target summary statistics
#'
#' summarize_diss takes a data frame to summarize data in a concise format.
#' The output closely resembles skimr::skim(), but with all the unnecessary information removed.
#'
#' @param data A data frame to summarize.
#' @param summary_vars A set of columns of interest.
#' @param summary_stats A set of summary statistics of interest.
#' @return Summarized object with groups intact.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tidyr)
#' library(purrr)
#' mtcars %>%
#'  group_by(cyl, gear) %>%
#'  summarize_diss(c("mpg", "disp"), c("mean", "sd", "median", "min", "max"))
#'
summarize_diss <- function(data, summary_vars, summary_stats) {
  data %>%
    dplyr::summarize(dplyr::across({{summary_vars}},
                     .fns = setNames(map(summary_stats, ~ match.fun(.x)),
                                     summary_stats),
                     na.rm = TRUE),
              .groups = "keep") %>%
    tidyr::pivot_longer(
      cols = dplyr::ends_with(summary_stats),
      names_to = c("variable", "summary"),
      names_sep = "_",
      values_to = "statistic"
    ) %>%
    tidyr::pivot_wider(names_from = summary, values_from = statistic)
}


