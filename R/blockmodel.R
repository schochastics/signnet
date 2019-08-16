#' @title Blockmodelling for signed networks
#' @description Finds blocks of nodes with intra-positive and inter-negative edges
#' @param g signed graph
#' @param k number of clusters
#' @param alpha see details
#' @return numeric vector of block assignments and the associated criterion value
#' @details The function minimizes P(C)=\eqn{\alpha}P+(1-\eqn{\alpha})N,
#' where P is the total number of negative ties within plus-sets and P be the total number of
#' positive ties between plus-sets. So far only the structural balance model is implemented.
#' @author David Schoch
#' @references
#' Doreian, Patrick and Andrej Mrvar (2009). Partitioning signed social networks. *Social Networks* 31(1) 1-11
#' @examples
#' library(igraph)
#' g <- sample_islands_signed(10,10,1,20)
#' clu <- signed_blockmodel(g,k = 10,alpha = 0.5)
#' table(clu$membership)
#' clu$criterion

#' @export
#'

signed_blockmodel <- function(g,k,alpha = 0.5){
  if(!"sign"%in%igraph::edge_attr_names(g)){
    stop("network does not have a sign edge attribute")
  }
  A <- igraph::get.adjacency(g,"both","sign",sparse = TRUE)
  init_cluster <- sample(0:(k-1),nrow(A),replace = TRUE)
  res <- optimBlocks1(A,init_cluster,k,alpha)
  res$membership <- res$membership+1
  res
}
