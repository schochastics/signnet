// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector triadCensusSign(NumericMatrix A, int n){

  int code=0;
  IntegerVector triads(729);

  for(int u=0;u<n;++u){
    for(int v=0;v<n;++v){
      for(int w=0;w<n;++w){
        if((u<v) & (v<w)){
          code = A(u,v)+3*A(u,w)+9*A(v,u)+27*A(v,w)+81*A(w,u)+243*A(w,v);
          triads[code] = triads[code] + 1;
        }
      }
    }
  }
  return triads;
}
