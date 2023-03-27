#include <Rcpp.h>
using namespace Rcpp;

//' Zernike expansion in Rcpp
//'
//' Calculates the Zernike expansion of a set of points in Cartesian coordinates.
//' This is a slightly faster version of the R code I wrote a while ago. The esoteric types like
//' Bhatia-Wolf will be added later.
//'
//' @param x NumericVector of x-coordinates.
//' @param y NumericVector of y-coordinates.
//' @param noll Integer specifying the Noll index of the Zernike polynomial.
//' @param normalized Boolean indicating whether the expansion should be normalized.
//' @return NumericVector of the Zernike expansion coefficients.
//'
//' @examples
//' x <- c(1, 2, 3)
//' y <- c(4, 5, 6)
//' zernike_expansion_rcpp(x, y, 3, TRUE)
//'
double factorial_z(int n) {
  double result = 1;
  for (int i = 2; i <= n; ++i) {
    result *= i;
  }
  return result;
}

double n_factor_z(int n, int m) {
  return sqrt((2 * n + 2) / (n - m + 1) / (n + m + 1));
}

// [[Rcpp::export]]
NumericVector zernike_expansion(NumericVector x, NumericVector y, int noll, bool normalized = true) {
  int n, m, j;
  double rho, theta, numerator, denominator;
  NumericVector Rnm(x.size()), result(x.size());

  for (int i = 0; i < x.size(); ++i) {
    rho = sqrt(pow(x[i], 2) + pow(y[i], 2));
    rho /= std::max(rho, 1e-15);
    theta = atan2(y[i], x[i]);
    j = noll;

    n = (int) (sqrt(2 * j - 1) + 0.5) - 1;

    if (n % 2 == 0) {
      m = 2 * (int) ((2 * j + 1 - n * (n + 1)) / 4);
    } else {
      m = 2 * (int) ((2 * (j + 1) - n * (n + 1)) / 4) - 1;
    }

    Rnm[i] = 0;
    for (int s = 0; s <= (n - m) / 2; ++s) {
      numerator = pow(-1, s) * factorial_z(n - s);
      denominator = factorial_z(s) * factorial_z((n + m) / 2 - s) * factorial_z((n - m) / 2 - s);
      Rnm[i] += numerator / denominator * pow(rho, n - 2 * s);
    }

    if (m > 0) {
      if (n % 2 == 0) {
        j = 2 * (int) ((2 * j + 1 - (n - 2) * ((n - 2) + 1)) / 4) + 2;
      } else {
        j = 2 * (int) ((2 * (j + 1) - (n - 1) * ((n - 1) + 1)) / 4) + 1;
      }
    }

    double Pnm;
    if (j % 2 == 0) {
      Pnm = Rnm[i] * cos(m * theta);
    } else {
      Pnm = Rnm[i] * sin(m * theta);
    }

    if (normalized) {
      result[i] = n_factor_z(n, m) * Pnm;
    } else {
      result[i] = Pnm;
    }
  }

  return result;
}

