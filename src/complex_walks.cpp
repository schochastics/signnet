// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

// product of complex matrices where real and imaginary parts of the
// summands are taken in absolute value: C(i,j) = sum_k |Re(a b)| + i |Im(a b)|
// [[Rcpp::export]]
arma::cx_mat cxmatmul(const arma::cx_mat &A, const arma::cx_mat &B) {
  const arma::uword n = A.n_rows;
  const arma::uword m = B.n_cols;
  const arma::uword p = A.n_cols;
  arma::cx_mat C(n, m);
  for (arma::uword j = 0; j < m; ++j) {
    for (arma::uword i = 0; i < n; ++i) {
      double re = 0;
      double im = 0;
      for (arma::uword k = 0; k < p; ++k) {
        std::complex<double> z = A(i, k) * B(k, j);
        re += std::abs(z.real());
        im += std::abs(z.imag());
      }
      C(i, j) = std::complex<double>(re, im);
    }
  }
  return C;
}
