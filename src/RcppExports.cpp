// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// arcDist
double arcDist(NumericVector x, NumericVector y, double r);
RcppExport SEXP _signnet_arcDist(SEXP xSEXP, SEXP ySEXP, SEXP rSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< double >::type r(rSEXP);
    rcpp_result_gen = Rcpp::wrap(arcDist(x, y, r));
    return rcpp_result_gen;
END_RCPP
}
// arcDistMat
NumericMatrix arcDistMat(NumericMatrix X, double r);
RcppExport SEXP _signnet_arcDistMat(SEXP XSEXP, SEXP rSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type X(XSEXP);
    Rcpp::traits::input_parameter< double >::type r(rSEXP);
    rcpp_result_gen = Rcpp::wrap(arcDistMat(X, r));
    return rcpp_result_gen;
END_RCPP
}
// cxmatmul
arma::cx_mat cxmatmul(arma::cx_mat A, arma::cx_mat B);
RcppExport SEXP _signnet_cxmatmul(SEXP ASEXP, SEXP BSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::cx_mat >::type A(ASEXP);
    Rcpp::traits::input_parameter< arma::cx_mat >::type B(BSEXP);
    rcpp_result_gen = Rcpp::wrap(cxmatmul(A, B));
    return rcpp_result_gen;
END_RCPP
}
// blockCriterion
double blockCriterion(arma::sp_mat A, IntegerVector clu, double alpha);
RcppExport SEXP _signnet_blockCriterion(SEXP ASEXP, SEXP cluSEXP, SEXP alphaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::sp_mat >::type A(ASEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type clu(cluSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    rcpp_result_gen = Rcpp::wrap(blockCriterion(A, clu, alpha));
    return rcpp_result_gen;
END_RCPP
}
// critUpdate
double critUpdate(arma::sp_mat A, int v, int from, int to, IntegerVector clu, double alpha);
RcppExport SEXP _signnet_critUpdate(SEXP ASEXP, SEXP vSEXP, SEXP fromSEXP, SEXP toSEXP, SEXP cluSEXP, SEXP alphaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::sp_mat >::type A(ASEXP);
    Rcpp::traits::input_parameter< int >::type v(vSEXP);
    Rcpp::traits::input_parameter< int >::type from(fromSEXP);
    Rcpp::traits::input_parameter< int >::type to(toSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type clu(cluSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    rcpp_result_gen = Rcpp::wrap(critUpdate(A, v, from, to, clu, alpha));
    return rcpp_result_gen;
END_RCPP
}
// optimBlocks1
List optimBlocks1(arma::sp_mat A, IntegerVector clu, int k, double alpha);
RcppExport SEXP _signnet_optimBlocks1(SEXP ASEXP, SEXP cluSEXP, SEXP kSEXP, SEXP alphaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::sp_mat >::type A(ASEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type clu(cluSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    rcpp_result_gen = Rcpp::wrap(optimBlocks1(A, clu, k, alpha));
    return rcpp_result_gen;
END_RCPP
}
// optimBlocksSim
List optimBlocksSim(arma::sp_mat A, IntegerVector clu, int k, double alpha);
RcppExport SEXP _signnet_optimBlocksSim(SEXP ASEXP, SEXP cluSEXP, SEXP kSEXP, SEXP alphaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::sp_mat >::type A(ASEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type clu(cluSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    rcpp_result_gen = Rcpp::wrap(optimBlocksSim(A, clu, k, alpha));
    return rcpp_result_gen;
END_RCPP
}
// blockCriterionS
double blockCriterionS(arma::sp_mat A, IntegerVector clu, double alpha, IntegerMatrix sgrp);
RcppExport SEXP _signnet_blockCriterionS(SEXP ASEXP, SEXP cluSEXP, SEXP alphaSEXP, SEXP sgrpSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::sp_mat >::type A(ASEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type clu(cluSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< IntegerMatrix >::type sgrp(sgrpSEXP);
    rcpp_result_gen = Rcpp::wrap(blockCriterionS(A, clu, alpha, sgrp));
    return rcpp_result_gen;
END_RCPP
}
// critUpdateS
double critUpdateS(arma::sp_mat A, int v, int from, int to, IntegerVector clu, double alpha, IntegerMatrix sgrp);
RcppExport SEXP _signnet_critUpdateS(SEXP ASEXP, SEXP vSEXP, SEXP fromSEXP, SEXP toSEXP, SEXP cluSEXP, SEXP alphaSEXP, SEXP sgrpSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::sp_mat >::type A(ASEXP);
    Rcpp::traits::input_parameter< int >::type v(vSEXP);
    Rcpp::traits::input_parameter< int >::type from(fromSEXP);
    Rcpp::traits::input_parameter< int >::type to(toSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type clu(cluSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< IntegerMatrix >::type sgrp(sgrpSEXP);
    rcpp_result_gen = Rcpp::wrap(critUpdateS(A, v, from, to, clu, alpha, sgrp));
    return rcpp_result_gen;
END_RCPP
}
// optimBlocksSimS
List optimBlocksSimS(arma::sp_mat A, IntegerVector clu, IntegerMatrix sgrp, double alpha);
RcppExport SEXP _signnet_optimBlocksSimS(SEXP ASEXP, SEXP cluSEXP, SEXP sgrpSEXP, SEXP alphaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::sp_mat >::type A(ASEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type clu(cluSEXP);
    Rcpp::traits::input_parameter< IntegerMatrix >::type sgrp(sgrpSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    rcpp_result_gen = Rcpp::wrap(optimBlocksSimS(A, clu, sgrp, alpha));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_signnet_arcDist", (DL_FUNC) &_signnet_arcDist, 3},
    {"_signnet_arcDistMat", (DL_FUNC) &_signnet_arcDistMat, 2},
    {"_signnet_cxmatmul", (DL_FUNC) &_signnet_cxmatmul, 2},
    {"_signnet_blockCriterion", (DL_FUNC) &_signnet_blockCriterion, 3},
    {"_signnet_critUpdate", (DL_FUNC) &_signnet_critUpdate, 6},
    {"_signnet_optimBlocks1", (DL_FUNC) &_signnet_optimBlocks1, 4},
    {"_signnet_optimBlocksSim", (DL_FUNC) &_signnet_optimBlocksSim, 4},
    {"_signnet_blockCriterionS", (DL_FUNC) &_signnet_blockCriterionS, 4},
    {"_signnet_critUpdateS", (DL_FUNC) &_signnet_critUpdateS, 7},
    {"_signnet_optimBlocksSimS", (DL_FUNC) &_signnet_optimBlocksSimS, 4},
    {NULL, NULL, 0}
};

RcppExport void R_init_signnet(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
