#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// Shared machinery for signed blockmodeling.
//
// A tie with sign a between blocks (cu, cv) is a violation if it disagrees
// with the block pattern sgrp(cu, cv). Negative ties in positive blocks are
// weighted by alpha, positive ties in negative blocks by (1 - alpha).
// Every non-zero entry of A is counted, so each undirected edge counts twice.
// Cluster ids are 0-based.

namespace {

struct Nbr {
  int u;
  double a;
};

struct Graph {
  int n;
  std::vector<std::vector<Nbr> > out; // v -> u, value A(v, u)
  std::vector<std::vector<Nbr> > in;  // u -> v, value A(u, v)
};

Graph make_graph(const arma::sp_mat &A) {
  Graph G;
  G.n = A.n_cols;
  G.out.resize(G.n);
  G.in.resize(G.n);
  for (arma::sp_mat::const_iterator it = A.begin(); it != A.end(); ++it) {
    int u = it.row();
    int v = it.col();
    if (u == v) {
      continue; // self-loops never change when a vertex moves
    }
    Nbr to_v = {v, *it};
    Nbr from_u = {u, *it};
    G.out[u].push_back(to_v);
    G.in[v].push_back(from_u);
  }
  return G;
}

inline double violation(double a, int expected, double alpha) {
  if (a < 0 && expected == 1) {
    return -a * alpha;
  }
  if (a > 0 && expected == -1) {
    return a * (1 - alpha);
  }
  return 0;
}

// cost of the ties incident to v if v were placed in cluster c
double vertex_cost(const Graph &G, int v, int c, const std::vector<int> &clu,
                   const IntegerMatrix &sgrp, double alpha) {
  double cost = 0;
  for (size_t i = 0; i < G.out[v].size(); ++i) {
    const Nbr &e = G.out[v][i];
    cost += violation(e.a, sgrp(c, clu[e.u]), alpha);
  }
  for (size_t i = 0; i < G.in[v].size(); ++i) {
    const Nbr &e = G.in[v][i];
    cost += violation(e.a, sgrp(clu[e.u], c), alpha);
  }
  return cost;
}

double criterion(const arma::sp_mat &A, const std::vector<int> &clu,
                 const IntegerMatrix &sgrp, double alpha) {
  double crit = 0;
  for (arma::sp_mat::const_iterator it = A.begin(); it != A.end(); ++it) {
    crit += violation(*it, sgrp(clu[it.row()], clu[it.col()]), alpha);
  }
  return crit;
}

// best-improvement local search; modifies clu in place
void greedy(const Graph &G, std::vector<int> &clu, const IntegerMatrix &sgrp,
            double alpha, int maxiter) {
  int k = sgrp.nrow();
  std::vector<int> sizes(k, 0);
  for (int v = 0; v < G.n; ++v) {
    sizes[clu[v]] += 1;
  }
  std::vector<double> cost(k);
  for (int iter = 0; iter < maxiter; ++iter) {
    double best_delta = -1e-9;
    int best_v = -1;
    int best_c = -1;
    for (int v = 0; v < G.n; ++v) {
      for (int c = 0; c < k; ++c) {
        cost[c] = vertex_cost(G, v, c, clu, sgrp, alpha);
      }
      for (int c = 0; c < k; ++c) {
        if (c == clu[v]) {
          continue;
        }
        double delta = cost[c] - cost[clu[v]];
        // ties are broken in favour of smaller target blocks
        if (delta < best_delta ||
            (best_v >= 0 && delta == best_delta && sizes[c] < sizes[best_c])) {
          best_delta = delta;
          best_v = v;
          best_c = c;
        }
      }
    }
    if (best_v < 0) {
      break;
    }
    sizes[clu[best_v]] -= 1;
    sizes[best_c] += 1;
    clu[best_v] = best_c;
  }
}

std::vector<int> to_std(const IntegerVector &clu, int k) {
  std::vector<int> res(clu.begin(), clu.end());
  for (size_t i = 0; i < res.size(); ++i) {
    if (res[i] < 0 || res[i] >= k) {
      stop("cluster ids must be between 0 and k - 1");
    }
  }
  return res;
}

List make_result(const arma::sp_mat &A, const std::vector<int> &clu,
            const IntegerMatrix &sgrp, double alpha) {
  IntegerVector membership(clu.begin(), clu.end());
  return List::create(Named("membership") = membership,
                      Named("criterion") = criterion(A, clu, sgrp, alpha));
}

} // namespace

// [[Rcpp::export]]
double blockCriterion(const arma::sp_mat &A, IntegerVector clu,
                      IntegerMatrix sgrp, double alpha) {
  return criterion(A, to_std(clu, sgrp.nrow()), sgrp, alpha);
}

// [[Rcpp::export]]
List blockGreedy(const arma::sp_mat &A, IntegerVector clu, IntegerMatrix sgrp,
                 double alpha, int maxiter) {
  Graph G = make_graph(A);
  std::vector<int> cl = to_std(clu, sgrp.nrow());
  greedy(G, cl, sgrp, alpha, maxiter);
  return make_result(A, cl, sgrp, alpha);
}

// simulated annealing followed by a greedy polish of the best solution
// [[Rcpp::export]]
List blockAnneal(const arma::sp_mat &A, IntegerVector clu, IntegerMatrix sgrp,
                 double alpha, double temp0, double cooling, double temp_min,
                 int iter_per_temp) {
  Graph G = make_graph(A);
  int k = sgrp.nrow();
  std::vector<int> cl = to_std(clu, k);
  std::vector<int> best = cl;

  if (k > 1 && G.n > 0) {
    double crit = criterion(A, cl, sgrp, alpha);
    double crit_best = crit;
    for (double temp = temp0; temp > temp_min; temp *= cooling) {
      for (int it = 0; it < iter_per_temp; ++it) {
        int v = static_cast<int>(unif_rand() * G.n);
        int to = static_cast<int>(unif_rand() * (k - 1));
        if (to >= cl[v]) {
          to += 1;
        }
        double delta = vertex_cost(G, v, to, cl, sgrp, alpha) -
                       vertex_cost(G, v, cl[v], cl, sgrp, alpha);
        if (delta <= 0 || unif_rand() < std::exp(-delta / temp)) {
          cl[v] = to;
          crit += delta;
          if (crit < crit_best - 1e-9) {
            crit_best = crit;
            best = cl;
          }
        }
      }
    }
  }
  greedy(G, best, sgrp, alpha, 100 * G.n);
  return make_result(A, best, sgrp, alpha);
}
