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


#' Summarize a numeric vector by quantiles within groups
#'
#' This function summarizes a numeric vector by quantiles within groups,
#' returning a data frame with the quantile values and their corresponding
#' probabilities.
#'
#' @seealso \href{https://speakerdeck.com/jennybc/row-oriented-workflows-in-r-with-the-tidyverse}{Jenny Bryan's talk}.
#'
#' @param x A numeric vector to be summarized by quantiles
#' @param probs A numeric vector of quantile probabilities to compute.
#'   Defaults to c(0.25, 0.5, 0.75).
#' @param ... Additional arguments to be passed to the quantile() function
#'   (e.g., na.rm = TRUE).
#'
#' @return A list containing a data frame with the quantile values and their
#'   corresponding probabilities.
#' @examples
#' library(dplyr)
#' library(tidyr)
#' iris %>%
#'   group_by(Species)  |>
#'   summarise(pl_qtile = summarize_enkeyed(Petal.Length)) |>
#'   unnest(pl_qtile)
#' @export
summarize_enkeyed <- function(x, probs = c(0.25, 0.5, 0.75), ...) {
  qtile <- quantile(x, probs = probs, ...)
  qtile <- setNames(qtile, paste0(probs*100, "%"))
  qtile <- as.data.frame(t(qtile))
  qtile$quantile <- rownames(qtile)
  qtile$quantile <- factor(qtile$quantile, levels = rownames(qtile))
  list(qtile)
}


