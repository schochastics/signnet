#' @title Blockmodeling for signed networks
#' @description Finds blocks of nodes with intra-positive and inter-negative edges
#' @param g igraph object with a sign edge attribute.
#' @param k number of blocks
#' @param alpha see details
#' @param annealing logical. if TRUE, use simulated annealing (Default: FALSE)
#' @return numeric vector of block assignments and the associated criterion value
#' @details The function minimizes P(C)=\eqn{\alpha}N+(1-\eqn{\alpha})P,
#' where N is the total number of negative ties within plus-sets and P be the total number of
#' positive ties between plus-sets. This function implements the structural balance model. That is,
#' all diagonal blocks are positive and off-diagonal blocks negative.
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
    if (!is_signed(g)) {
        stop("network is not a signed graph")
    }
    if (missing(k)) {
        stop('argument "k" is missing, with no default')
    }
    A <- igraph::as_adj(g, type = "both", attr = "sign", sparse = TRUE)
    if (!annealing) {
        init_cluster <- sample(0:(k - 1), nrow(A), replace = TRUE)
        res <- optimBlocks1(A, init_cluster, k, alpha)
        res$membership <- res$membership + 1
    } else {
        init_cluster <- sample(1:k, nrow(A), replace = TRUE)
        tmp <- stats::optim(
            par = init_cluster, fn = blockCriterion1, A = A, alpha = alpha, k = k, gr = genclu, method = "SANN",
            control = list(
                maxit = 50000, temp = 100, tmax = 500, trace = FALSE,
                REPORT = 5
            )
        )
        tmp <- stats::optim(
            par = tmp$par, fn = blockCriterion1, A = A, alpha = alpha, k = k, gr = genclu, method = "SANN",
            control = list(
                maxit = 5000, temp = 5, tmax = 500, trace = FALSE,
                REPORT = 5
            )
        )

        res <- list(membership = tmp$par, criterion = tmp$value)
    }
    res
}


#' @title Generalized blockmodeling for signed networks
#' @description Finds blocks of nodes with specified inter/intra group ties
#' @param g igraph object with a sign edge attribute.
#' @param blockmat Integer Matrix. Specifies the inter/intra group patterns of ties
#' @param alpha see details
#' @return numeric vector of block assignments and the associated criterion value
#' @details The function minimizes P(C)=\eqn{\alpha}N+(1-\eqn{\alpha})P,
#' where N is the total number of negative ties within plus-sets and P be the total number of
#' positive ties between plus-sets. This function implements the generalized model. For the structural balance
#' version see [signed_blockmodel].
#' @author David Schoch
#' @references
#' Doreian, Patrick and Andrej Mrvar (2009). Partitioning signed social networks. *Social Networks* 31(1) 1-11
#' @examples
#' library(igraph)
#' # create a signed network with three groups and different inter/intra group ties
#' g1 <- g2 <- g3 <- graph.full(5)
#'
#' V(g1)$name <- as.character(1:5)
#' V(g2)$name <- as.character(6:10)
#' V(g3)$name <- as.character(11:15)
#'
#' g <- Reduce("%u%", list(g1, g2, g3))
#' E(g)$sign <- 1
#' E(g)$sign[1:10] <- -1
#' g <- add.edges(g, c(rbind(1:5, 6:10)), attr = list(sign = -1))
#' g <- add.edges(g, c(rbind(1:5, 11:15)), attr = list(sign = -1))
#' g <- add.edges(g, c(rbind(11:15, 6:10)), attr = list(sign = 1))
#'
#' # specify the link patterns between groups
#' blockmat <- matrix(c(1, -1, -1, -1, 1, 1, -1, 1, -1), 3, 3, byrow = TRUE)
#' signed_blockmodel_general(g, blockmat, 0.5)
#' @export
#'

signed_blockmodel_general <- function(g, blockmat, alpha = 0.5) {
    if (!is_signed(g)) {
        stop("network is not a signed graph")
    }
    if (missing(blockmat)) {
        stop('argument "blockmat" is missing, with no default')
    }
    if (!all(blockmat %in% c(-1, 1))) {
        stop('"blockmat" may only contain -1 and 1')
    }
    A <- igraph::as_adj(g, type = "both", attr = "sign", sparse = TRUE)
    init_cluster <- sample(0:(nrow(blockmat) - 1), nrow(A), replace = TRUE)
    res <- optimBlocksSimS(A, init_cluster, blockmat, alpha)
    res$membership <- res$membership + 1
    res
}

# helper function to create a new solution during simulated annealing
genclu <- function(blocks, A, alpha, k) {
    v <- sample(seq_along(blocks), 1)
    clu <- 1:k
    clu <- clu[-blocks[v]]
    new <- sample(clu, 1)
    blocks[v] <- new
    blocks
}
