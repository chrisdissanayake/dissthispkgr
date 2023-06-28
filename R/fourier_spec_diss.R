#' Plot Fourier spectrogram of a signal
#'
#' My aim in writing this is to make the TF signal stand out and pleasing.
#'
#' @param tbl A data frame containing the time and signal columns
#' @param title A character string specifying the title of the spectrogram (default is "signal")
#'
#' @return A ggplot object representing the Fourier spectrogram of the signal
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' library(tidyr)
#'
#' # Create a sine wave with frequency of 100 Hz and amplitude of 1
#' time <- seq(0, 1000, length.out = 1000)
#' signal <- sin(2 * pi * 100 * time)
#'
#' # Add some noise to the signal
#' set.seed(123)
#' signal <- signal + rnorm(length(signal), sd = 0.01)
#'
#' df <- data.frame(time = time, signal = signal)
#'
#' plot(df, type = "o")
#' fourier_spec_diss(df)
#'
#' @export
fourier_spec_diss <- function(tbl, nfft = 4000, ns = 30, nov = 0, title = "signal") {
  dt <- mean(diff(tbl$time))
  ft <- list(nfft = nfft, ns = ns, nov = nov)
  freq_span <- c(0, 1/(dt + 2) - 0.01)

  sig <- tbl$signal
  time_span = c(dt, length(sig) * dt)
  sig = sig[(time_span[1]/dt):(time_span[2]/dt)]

  ev = hht::EvolutiveFFT(sig, dt, ft, freq_span, taper = 0.05)
  # amp_span = c(min(ev$z[ev$z > -Inf]), max(ev$z[ev$z < Inf]))

  x <- ev$x + time_span[1]
  y <- seq(freq_span[1], freq_span[2], by = ev$y[2] - ev$y[1])

  img = list(z = array(0, dim = c(length(x), length(y))), x = x, y = y)

  img$z[, y >= min(ev$y) & y <= max(ev$y)] = ev$z[, ev$y >= freq_span[1] & ev$y <= freq_span[2]]

  expand_grid(y = y, x = x) |>
    mutate(z = as.vector(img$z)) |>
    ggplot(aes(x, y)) +
    geom_tile(aes(fill = z)) +
    # geom_hline(yintercept = 0.1667, col = "white", linetype = "dashed") +
    scale_fill_gradientn(colours = rainbow(500, start = 0, end = 5/6), breaks = scales::breaks_pretty(3)) +
    labs(y = "Frequency (kHz)",
         x = "Time") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white"),
          legend.key.size = unit(0.3, "cm") )
}
