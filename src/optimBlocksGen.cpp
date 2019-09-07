#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export]]
double blockCriterionS(arma::sp_mat A,IntegerVector clu,double alpha,IntegerMatrix sgrp){
  double P = 0;
  double N = 0;

  for (arma::sp_mat::const_iterator i = A.begin(); i != A.end(); ++i) {
    if((clu[i.row()]==clu[i.col()]) & (*i!=sgrp(clu[i.row()],clu[i.col()]))){
      if(sgrp(clu[i.row()],clu[i.col()])==1){
        P+=1;
      } else{
        N+=1;
      }

    }
    if((clu[i.row()]!=clu[i.col()]) & (*i!=sgrp(clu[i.row()],clu[i.col()]))){
      if(sgrp(clu[i.row()],clu[i.col()])==1){
        P+=1;
      } else{
        N+=1;
      }
    }
  }
  return alpha*N+(1-alpha)*P;
}

// [[Rcpp::export]]
double critUpdateS(arma::sp_mat A, int v, int from, int to,IntegerVector clu,double alpha,IntegerMatrix sgrp){
  double P = 0;
  double N = 0;
  arma::sp_mat::const_col_iterator start = A.begin_col(v);
  arma::sp_mat::const_col_iterator end = A.end_col(v);
  for(arma::sp_mat::const_col_iterator j = start; j != end; ++j){

    if((A(j.row(),v))==sgrp(from,clu[j.row()])){
      if(sgrp(from,clu[j.row()])==1){
        P+=1;
      } else{
        N+=1;
      }
    }
    if((A(j.row(),v))!=sgrp(from,clu[j.row()])){
      if(sgrp(from,clu[j.row()])==1){
        P-=1;
      } else{
        N-=1;
      }
    }
    if((A(j.row(),v))==sgrp(to,clu[j.row()])){
      if(sgrp(to,clu[j.row()])==1){
        P-=1;
      } else{
        N-=1;
      }
    }
    if((A(j.row(),v))!=sgrp(to,clu[j.row()])){
      if(sgrp(to,clu[j.row()])==1){
        P+=1;
      } else{
        N+=1;
      }
    }
  }

  // A=A.t();
  // start = A.begin_col(v);
  // end = A.end_col(v);
  // for(arma::sp_mat::const_col_iterator j = start; j != end; ++j){
  //
  //   if((A(j.row(),v))==sgrp(from,clu[j.row()])){
  //     if(sgrp(from,clu[j.row()])==1){
  //       P+=1;
  //     } else{
  //       N+=1;
  //     }
  //   }
  //   if((A(j.row(),v))!=sgrp(from,clu[j.row()])){
  //     if(sgrp(from,clu[j.row()])==1){
  //       P-=1;
  //     } else{
  //       N-=1;
  //     }
  //   }
  //   if((A(j.row(),v))==sgrp(to,clu[j.row()])){
  //     if(sgrp(to,clu[j.row()])==1){
  //       P-=1;
  //     } else{
  //       N-=1;
  //     }
  //   }
  //   if((A(j.row(),v))!=sgrp(to,clu[j.row()])){
  //     if(sgrp(to,clu[j.row()])==1){
  //       P+=1;
  //     } else{
  //       N+=1;
  //     }
  //   }
  // }
  return alpha*N+(1-alpha)*P;

}

// [[Rcpp::export]]
List optimBlocksSimS(arma::sp_mat A,IntegerVector clu, IntegerMatrix sgrp, double alpha){
  int k = sgrp.nrow();
  double crit = blockCriterionS(A,clu,alpha,sgrp);
  double crit_min =crit;
  int n = A.n_cols;
  int v;
  int newc;
  double deltaC;
  int max_iter = n*n;
  double temp = 100;
  IntegerVector clu_min(n);

  IntegerVector cluSizes(k);
  for(int i=0; i<n;++i){
    cluSizes[clu[i]]+=1;
  }

  while(temp>0.1){
    for(int q=0;q<max_iter;++q){
      v = floor(R::runif(0,1)*n);
      newc = floor(R::runif(0,1)*k);
      if(newc==clu[v]){
        continue;
      }
      deltaC = critUpdateS(A, v, clu[v], newc,clu,alpha,sgrp);
      if(deltaC<0){
        cluSizes[clu[v]]-=1;
        cluSizes[newc]+=1;
        clu[v]=newc;
        // stuck=0;
        crit+=deltaC;
        if(crit<crit_min){
          crit_min = crit;
          clu_min = clu;
        }
      } else{
        double p = exp(-(crit+deltaC-crit_min)/temp);
        if(R::runif(0,1)<=p){
          crit = crit+deltaC;
          clu[v]=newc;
          // stuck = 0;
        } else {
          // stuck+=1;
        }
      }
    }

    temp = 0.99*temp;
  }

  return Rcpp::List::create(Rcpp::Named("membership") = clu_min,
                            Rcpp::Named("criterion") = crit_min);
}

//
// if((clu[j.row()]==from) & (A(j.row(),v)!=sgrp(from,from))){
//   N-=1;
//   continue;
// }
// if((clu[j.row()]==to) & (A(j.row(),v)!=sgrp(to,to))){
//   N+=1;
//   continue;
// }
// if((clu[j.row()]==to) & (A(j.row(),v)==sgrp(to,to))){
//   P-=1;
//   continue;
// }
// if((clu[j.row()]==from) & (A(j.row(),v)==sgrp(from,to))){
//   P+=1;
//   // continue;
// }
