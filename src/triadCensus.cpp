#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;


// [[Rcpp::export]]
IntegerVector triadCensusSign(NumericMatrix A, int n)
{

  int code = 0;
  IntegerVector triads(729);

  for (int u = 0; u < n; ++u)
  {
    for (int v = 0; v < n; ++v)
    {
      for (int w = 0; w < n; ++w)
      {
        if ((u < v) && (v < w))
        {
          code = A(u, v) + 3 * A(u, w) + 9 * A(v, u) + 27 * A(v, w) + 81 * A(w, u) + 243 * A(w, v);
          triads[code] = triads[code] + 1;
        }
      }
    }
  }
  return triads;
}

// [[Rcpp::export]]
DoubleVector triadCensusSign1(const arma::sp_mat &A, List adj, int n)
{
  long long code = 0;
  DoubleVector triads(729);
  for (int u = 0; u < n; u++)
  {
    IntegerVector Nu = as<IntegerVector>(adj[u]);
    int nu = Nu.length();
    for (int j = 0; j < nu; j++)
    {
      int v = Nu[j];
      if (u < v)
      {
        IntegerVector Nv = as<IntegerVector>(adj[v]);
        IntegerVector S = union_(Nu, Nv);
        IntegerVector uv = {u, v};
        S = setdiff(S, uv);
        code = (A(u, v) + 1) + 3 + 9 * (A(v, u) + 1) + 27 + 81 + 243;
        triads[code] = triads[code] + n - S.length() - 2;
        for (int k = 0; k < S.length(); k++)
        {
          int w = S[k];
          if ((v < w) || ((u < w) && (w < v) && (A(u, w) == 0) && (A(w, u) == 0)))
          {
            code = (A(u, v) + 1) + 3 * (A(u, w) + 1) + 9 * (A(v, u) + 1) + 27 * (A(v, w) + 1) + 81 * (A(w, u) + 1) + 243 * (A(w, v) + 1);
            triads[code] = triads[code] + 1;
          }
        }
      }
    }
  }
  return triads;
}

