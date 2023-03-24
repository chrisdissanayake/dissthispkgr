#' Calculate the Marcenko-Pastur Probability Density Function (PDF)
#'
#' Given a variance value, q, and number of points, calculates the PDF for the
#' Marcenko-Pastur distribution. The q ratio is explicit for clarity (for me).
#'
#' @param variance The variance value for the Marcenko-Pastur distribution.
#' @param q The ratio of variables to observations.
#' @param pts The number of points to use in the PDF.
#'
#' @return A data.frame containing the Marcenko-Pastur PDF values and their
#' corresponding index values.
#' @seealso \url{https://en.wikipedia.org/wiki/Marchenko%E2%80%93Pastur_distribution#Application_to_correlation_matrices}
#' @export
#'
#' @examples
#' pdf <- marchenko_pastur_pdf(variance = 1, q = 0.5, pts = 100)
#' plot(pdf$index, pdf$pdf, type = "o")
marchenko_pastur_pdf <- function(variance, q, pts) {
  lambda_min <- variance*(1 - (1/q)^0.5)^2
  lambda_max <- variance*(1 + (1/q)^0.5)^2
  eigen_val <- seq(lambda_min, lambda_max, length.out = pts)
  pdf <- q/(2*pi*variance*eigen_val)*((lambda_max - eigen_val)*(eigen_val - lambda_min))^0.5
  data.frame(pdf, index = eigen_val)
}
