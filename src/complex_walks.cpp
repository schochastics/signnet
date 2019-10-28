// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

// [[Rcpp::export]]
arma::cx_mat cxmatmul(arma::cx_mat A,arma::cx_mat B) {
  int n=A.n_rows;
  arma::cx_mat C(n,n);
  arma::cx_vec tmp(n);
  arma::vec Repart(n);
  arma::vec Impart(n);

  for(int i=0;i<n;++i){
    for(int j=0;j<n;++j){
      for(int k=0;k<n;++k){
        tmp[k] = A(i,k)*B(k,j);
      }
      Repart = abs(real(tmp));
      Impart = abs(imag(tmp));
      tmp = arma::cx_vec(Repart,Impart);
      C(i,j) = sum(tmp);
    }
  }
  return C;
}

