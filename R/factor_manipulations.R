#' Plot a categorical variable by lumping infrequent levels
#'
#' Returns a bar plot of the frequency distribution of the specified column.
#'
#' @param data A data frame containing the column to be plotted.
#' @param col_name A character string specifying the column name to be plotted.
#' @param min_val An integer specifying the minimum frequency count for a level to be kept in the plot.
#' @return A bar plot of the specified column, with relabeled and lumped infrequent levels.
#' @import dplyr
#' @import forcats
#' @import ggplot2
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' library(forcats)
#' starwars |> plot_fct_lump("skin_color", 3)
plot_fct_lump <- function(data, col_name, min_val) {
  data %>%
    mutate(
      !!sym(col_name) := !!sym(col_name) %>%
        fct_na_value_to_level() %>%
        fct_infreq() %>%
        fct_lump_min(min = min_val, other_level = "(Other)") %>%
        fct_rev()
    ) %>%
    ggplot(aes(y = !!sym(col_name))) +
    geom_bar() +
    labs(y = col_name)
}
