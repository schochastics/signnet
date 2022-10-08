#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export]]
double blockCriterion(arma::sp_mat A,IntegerVector clu,double alpha){
  double P = 0;
  double N = 0;

  for (arma::sp_mat::const_iterator i = A.begin(); i != A.end(); ++i) {
    if((clu[i.row()]==clu[i.col()]) && (*i==-1)){
      N+=1;
    }
    if((clu[i.row()]!=clu[i.col()]) && (*i==1)){
      P+=1;
    }
  }
  return alpha*N+(1-alpha)*P;
}

// [[Rcpp::export]]
double critUpdate(arma::sp_mat A, int v, int from, int to,IntegerVector clu,double alpha){
  double P = 0;
  double N = 0;
  arma::sp_mat::const_col_iterator start = A.begin_col(v);
  arma::sp_mat::const_col_iterator end = A.end_col(v);
  for(arma::sp_mat::const_col_iterator j = start; j != end; ++j){

    if((clu[j.row()]==from) && (A(j.row(),v)==-1)){
      N-=1;
    }
    if((clu[j.row()]==to) && (A(j.row(),v)==-1)){
      N+=1;
    }
    if((clu[j.row()]==to) && (A(j.row(),v)==1)){
      P-=1;
    }
    if((clu[j.row()]==from) && (A(j.row(),v)==1)){
      P+=1;
    }
  }
  A=A.t();
  start = A.begin_col(v);
  end = A.end_col(v);
  for(arma::sp_mat::const_col_iterator j = start; j != end; ++j){

    if((clu[j.row()]==from) && (A(j.row(),v)==-1)){
      N-=1;
    }
    if((clu[j.row()]==to) && (A(j.row(),v)==-1)){
      N+=1;
    }
    if((clu[j.row()]==to) && (A(j.row(),v)==1)){
      P-=1;
    }
    if((clu[j.row()]==from) && (A(j.row(),v)==1)){
      P+=1;
    }
  }
  return alpha*N+(1-alpha)*P;

}

// [[Rcpp::export]]
List optimBlocks1(arma::sp_mat A,IntegerVector clu, int k, double alpha){

  double crit = blockCriterion(A,clu,alpha);
  int n = A.n_cols;
  int maxiter = n*n;
  int iter=0;

  IntegerVector cluSizes(k);
  for(int i=0; i<n;++i){
    cluSizes[clu[i]]+=1;
  }

  NumericMatrix updateMat(n,k);
  IntegerVector mover(2);
  while(iter<maxiter){
    double minDelta = 1000;
    double minOptim = 1000;
    for(int i=0;i<n;++i){
      for(int j=0;j<k;++j){
        updateMat(i,j) = critUpdate(A, i, clu[i], j,clu,alpha);
        if(updateMat(i,j)+cluSizes[j]<minOptim){
          minDelta = updateMat(i,j);
          minOptim = minDelta + cluSizes[j];
          mover[0] = i;
          mover[1] = j;
        }
      }
    }
    if(minDelta<0){
      cluSizes[clu[mover[0]]]-=1;
      cluSizes[mover[1]]+=1;
      clu[mover[0]]= mover[1];
      crit+=minDelta;

    } else{
      break;
    }
    iter+=1;
  }
  return Rcpp::List::create(Rcpp::Named("membership") = clu,
                            Rcpp::Named("criterion")=crit);
}

// [[Rcpp::export]]
double blockCriterion1(IntegerVector clu,arma::sp_mat A,double alpha,int k){
  double P = 0;
  double N = 0;

  for (arma::sp_mat::const_iterator i = A.begin(); i != A.end(); ++i) {
    if((clu[i.row()]==clu[i.col()]) && (*i==-1)){
      N+=1;
    }
    if((clu[i.row()]!=clu[i.col()]) && (*i==1)){
      P+=1;
    }
  }
  return alpha*N+(1-alpha)*P;
}
