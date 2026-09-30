#' @title balancedness of signed network
#' @description Implements several indices to assess the balancedness of a network.
#'
#' @param g igraph object with a sign edge attribute.
#' @param method string indicating the method to be used. See details for options
#' @details The method parameter can be one of
#' \describe{
#'   \item{*triangles*}{Fraction of balanced triangles. Maximal (=1) if all triangles are balanced.}
#'   \item{*walk*}{\eqn{\sum exp(\lambda_i) / \sum exp(\mu_i)}} where \eqn{\lambda_i} are the eigenvalues of the
#'   signed adjacency matrix and \eqn{\mu_i} of the unsigned adjacency matrix. Maximal (=1) if all walks are balanced.
#'   \item{*frustration*}{The frustration index assumes that the network can be partitioned into two groups, where intra group edges are positive and inter group edges are negative. The index is defined as the sum of intra group negative and inter group positive edges. Note that the problem is NP complete and only an upper bound is returned (based on simulated annealing). Exact methods can be found in the work of Aref. The index is normalized such that it is maximal (=1) if the network is balanced.}
#' }
#' @return numeric balancedness score between 0 and 1. For `method = "triangles"`, `NA` if the network has no triangles.
#' @author David Schoch
#' @references
#' Estrada, E. (2019). Rethinking structural balance in signed social networks. *Discrete Applied Mathematics*.
#'
#' Samin Aref, Mark C Wilson (2018). Measuring partial balance in signed networks. *Journal of Complex Networks*, 6(4): 566–595, https://doi.org/10.1093/comnet/cnx044
#' @examples
#' library(igraph)
#' g <- make_full_graph(4)
#' E(g)$sign <- c(-1, 1, 1, -1, -1, 1)
#'
#' balance_score(g, method = "triangles")
#' balance_score(g, method = "walk")
#' @export
balance_score <- function(g, method = "triangles") {
  method <- match.arg(method, c("triangles", "walk", "frustration"))
  check_signed(g, directed = FALSE)

  if (method == "triangles") {
    tria_count <- count_signed_triangles(g)
    if (sum(tria_count) == 0) {
      return(NA_real_)
    }
    return(unname((tria_count["+++"] + tria_count["+--"]) / sum(tria_count)))
  } else if (method == "walk") {
    A <- as_adj_signed(g)
    EigenS <- eigen(A, symmetric = TRUE, only.values = TRUE)$values
    EigenU <- eigen(abs(A), symmetric = TRUE, only.values = TRUE)$values
    # log-sum-exp to avoid overflow for large eigenvalues
    return(exp(log_sum_exp(EigenS) - log_sum_exp(EigenU)))
  } else if (method == "frustration") {
    clu <- signed_blockmodel(g, k = 2, alpha = 0.5, annealing = TRUE)
    return(1 - clu$criterion / (igraph::ecount(g) / 2))
  }
}

#' @title Exact frustration index of a signed network
#' @description Computes the exact frustration index of a signed network using linear programming
#'
#' @param g signed network
#' @param ... additional parameters for the ompr solver
#' @details The frustration index indicates the minimum number of edges whose removal results in a balance
#' network. The function needs the following packages to be installed: `ompr`, `ompr.roi`,`ROI`, and `ROI.plugin.glpk`.
#' The function Implements the AND model in Aref et al., 2020
#' @return list containing the frustration index and the bipartition of nodes
#' @author David Schoch
#' @references
#'
#' Aref, Samin, Andrew J. Mason, and Mark C. Wilson. "Computing the line index of balance using linear programming optimisation."
#' Optimization problems in graph theory. Springer, Cham, 2018. 65-84.
#'
#' Aref, Samin, Andrew J. Mason, and Mark C. Wilson. "A modeling and computational study of the frustration index in signed networks." Networks 75.1 (2020): 95-110.
#' @examplesIf all(vapply(c("ompr", "ompr.roi", "ROI", "ROI.plugin.glpk"), requireNamespace, logical(1), quietly = TRUE))
#' data("tribes")
#' frustration_exact(tribes)
#' @export

frustration_exact <- function(g, ...) {
  check_signed(g, directed = FALSE)

  for (pkg in c("ompr", "ompr.roi", "ROI", "ROI.plugin.glpk")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(
        "the package '",
        pkg,
        "' is needed for this function to work",
        call. = FALSE
      )
    }
  }

  A <- as_adj_signed(g)
  n <- igraph::vcount(g)

  x <- matrix(0, n, n)
  y <- rep(0, n)
  i <- j <- 0
  # AND model
  result <- ompr::MIPModel()
  result <- ompr::add_variable(result, y[i], i = 1:n, type = "binary")
  result <- ompr::add_variable(
    result,
    x[i, j],
    i = 1:n,
    j = 1:n,
    i < j & A[i, j] != 0,
    type = "binary"
  )
  result <- ompr::set_objective(
    result,
    ompr::sum_over(
      y[i] + y[j] - 2 * x[i, j],
      i = 1:n,
      j = 1:n,
      i < j & A[i, j] == 1
    ) +
      ompr::sum_over(
        1 - (y[i] + y[j] - 2 * x[i, j]),
        i = 1:n,
        j = 1:n,
        i < j & A[i, j] == -1
      ),
    "min"
  )
  result <- ompr::add_constraint(
    result,
    x[i, j] <= y[i],
    i = 1:n,
    j = 1:n,
    i < j & A[i, j] == 1
  )
  result <- ompr::add_constraint(
    result,
    x[i, j] <= y[j],
    i = 1:n,
    j = 1:n,
    i < j & A[i, j] == 1
  )
  result <- ompr::add_constraint(
    result,
    x[i, j] >= y[i] + y[j] - 1,
    i = 1:n,
    j = 1:n,
    i < j & A[i, j] == -1
  )
  result <- ompr::solve_model(result, ompr.roi::with_ROI(solver = "glpk", ...))

  partition <- ompr::get_solution(result, y[i])
  partition <- partition$value
  frustration <- result$objective_value

  list(frustration = frustration, partition = partition)
}

log_sum_exp <- function(x) {
  m <- max(x)
  m + log(sum(exp(x - m)))
}
