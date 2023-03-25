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


