#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
double arcDist(NumericVector x,NumericVector y, double r) {
  double pi = acos(-1);
  double c = sqrt((x[0]-y[0])*(x[0]-y[0])+(x[1]-y[1])*(x[1]-y[1]));
  double theta = acos((2*r*r-c*c)/(2*r*r));
  return 2*pi*r*theta/(2*pi);
}


// [[Rcpp::export]]
NumericMatrix arcDistMat(NumericMatrix X,double r) {
  int n = X.nrow();
  NumericMatrix D(n,n);
  for(int i=0;i<n;i++){
    for(int j=0;j<n;j++){
      D(i,j)=arcDist(X(i,_),X(j,_),r);
    }
  }
  return D;
}
