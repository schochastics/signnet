#' @title Blockmodeling for signed networks
#' @description Finds blocks of nodes with intra-positive and inter-negative edges
#' @param g igraph object with a sign edge attribute.
#' @param k number of blocks
#' @param alpha see details
#' @param annealing logical. if TRUE, use simulated annealing followed by a greedy local search.
#' If FALSE, only use the greedy local search (Default: FALSE)
#' @return list with the block assignments (`membership`) and the associated criterion value (`criterion`)
#' @details The function minimizes P(C)=\eqn{\alpha}N+(1-\eqn{\alpha})P,
#' where N is the total number of negative ties within plus-sets and P be the total number of
#' positive ties between plus-sets. This function implements the structural balance model. That is,
#' all diagonal blocks are positive and off-diagonal blocks negative.
#' Ties are counted per entry of the adjacency matrix, so each undirected tie counts twice.
#' Both algorithms start from a random partition, so results can differ between runs. Use [set.seed()] for reproducible results.
#' For the generalized version see [signed_blockmodel_general].
#' @author David Schoch
#' @references
#' Doreian, Patrick and Andrej Mrvar (2009). Partitioning signed social networks. *Social Networks* 31(1) 1-11
#' @examples
#' library(igraph)
#'
#' g <- sample_islands_signed(10, 10, 1, 20)
#' clu <- signed_blockmodel(g, k = 10, alpha = 0.5)
#' table(clu$membership)
#' clu$criterion
#'
#' # Using simulated annealing (less change of getting trapped in local optima)
#' data("tribes")
#' clu <- signed_blockmodel(tribes, k = 3, alpha = 0.5, annealing = TRUE)
#' table(clu$membership)
#' clu$criterion
#' @export
signed_blockmodel <- function(g, k, alpha = 0.5, annealing = FALSE) {
  check_signed(g)
  if (missing(k)) {
    stop('argument "k" is missing, with no default')
  }
  n <- igraph::vcount(g)
  if (!is.numeric(k) || length(k) != 1 || k < 1 || k != round(k) || k > n) {
    stop('"k" must be an integer between 1 and the number of vertices')
  }
  check_alpha(alpha)
  blockmat <- 2 * diag(k) - 1
  run_blockmodel(g, blockmat, alpha, annealing)
}

#' @title Generalized blockmodeling for signed networks
#' @description Finds blocks of nodes with specified inter/intra group ties
#' @param g igraph object with a sign edge attribute.
#' @param blockmat Integer Matrix. Specifies the inter/intra group patterns of ties. Must be square,
#' contain only -1 and 1 and be symmetric for undirected networks.
#' @param alpha see details
#' @return list with the block assignments (`membership`) and the associated criterion value (`criterion`)
#' @details The function minimizes P(C)=\eqn{\alpha}N+(1-\eqn{\alpha})P,
#' where N is the total number of negative ties within positive blocks and P be the total number of
#' positive ties within negative blocks. This function implements the generalized model. For the structural balance
#' version see [signed_blockmodel].
#' Ties are counted per entry of the adjacency matrix, so each undirected tie counts twice.
#' The optimization uses simulated annealing followed by a greedy local search and starts from a random partition.
#' Use [set.seed()] for reproducible results.
#' @author David Schoch
#' @references
#' Doreian, Patrick and Andrej Mrvar (2009). Partitioning signed social networks. *Social Networks* 31(1) 1-11
#' @examples
#' library(igraph)
#' # create a signed network with three groups and different inter/intra group ties
#' g1 <- g2 <- g3 <- make_full_graph(5)
#'
#' V(g1)$name <- as.character(1:5)
#' V(g2)$name <- as.character(6:10)
#' V(g3)$name <- as.character(11:15)
#'
#' g <- Reduce("%u%", list(g1, g2, g3))
#' E(g)$sign <- 1
#' E(g)$sign[1:10] <- -1
#' g <- add_edges(g, c(rbind(1:5, 6:10)), attr = list(sign = -1))
#' g <- add_edges(g, c(rbind(1:5, 11:15)), attr = list(sign = -1))
#' g <- add_edges(g, c(rbind(11:15, 6:10)), attr = list(sign = 1))
#'
#' # specify the link patterns between groups
#' blockmat <- matrix(c(1, -1, -1, -1, 1, 1, -1, 1, -1), 3, 3, byrow = TRUE)
#' signed_blockmodel_general(g, blockmat, 0.5)
#' @export
#'

signed_blockmodel_general <- function(g, blockmat, alpha = 0.5) {
  check_signed(g)
  if (missing(blockmat)) {
    stop('argument "blockmat" is missing, with no default')
  }
  if (!is.matrix(blockmat) || nrow(blockmat) != ncol(blockmat)) {
    stop('"blockmat" must be a square matrix')
  }
  if (!all(blockmat %in% c(-1, 1))) {
    stop('"blockmat" may only contain -1 and 1')
  }
  if (!igraph::is_directed(g) && !isSymmetric(unname(blockmat))) {
    stop('"blockmat" must be symmetric for undirected networks')
  }
  if (nrow(blockmat) > igraph::vcount(g)) {
    stop('"blockmat" cannot have more blocks than the network has vertices')
  }
  check_alpha(alpha)
  run_blockmodel(g, blockmat, alpha, annealing = TRUE)
}

check_alpha <- function(alpha) {
  if (!is.numeric(alpha) || length(alpha) != 1 || alpha < 0 || alpha > 1) {
    stop('"alpha" must be a number between 0 and 1')
  }
}

run_blockmodel <- function(g, blockmat, alpha, annealing) {
  A <- as_adj_signed(g, sparse = TRUE)
  n <- nrow(A)
  k <- nrow(blockmat)
  storage.mode(blockmat) <- "integer"
  init_cluster <- sample.int(k, n, replace = TRUE) - 1L
  if (annealing) {
    res <- blockAnneal(
      A,
      init_cluster,
      blockmat,
      alpha,
      temp0 = 10,
      cooling = 0.99,
      temp_min = 0.01,
      iter_per_temp = max(n * k, 100L)
    )
  } else {
    res <- blockGreedy(A, init_cluster, blockmat, alpha, maxiter = 100L * n)
  }
  res$membership <- res$membership + 1L
  res
}
