#' @title Blockmodelling for signed networks
#' @description Finds blocks of nodes with intra-positive and inter-negative edges
#' @param g signed graph
#' @param k number of clusters
#' @param alpha see details
#' @param annealing logical. if TRUE, use simulated annealing (Default: FALSE)
#' @return numeric vector of block assignments and the associated criterion value
#' @details The function minimizes P(C)=\eqn{\alpha}P+(1-\eqn{\alpha})N,
#' where P is the total number of negative ties within plus-sets and P be the total number of
#' positive ties between plus-sets. So far only the structural balance model is implemented.
#' @author David Schoch
#' @references
#' Doreian, Patrick and Andrej Mrvar (2009). Partitioning signed social networks. *Social Networks* 31(1) 1-11
#' @examples
#' library(igraph)
#'
#' g <- sample_islands_signed(10,10,1,20)
#' clu <- signed_blockmodel(g,k = 10,alpha = 0.5)
#' table(clu$membership)
#' clu$criterion
#'
#' # Using simulated annealing (less change of getting trapped in local optima)
#' data("tribes")
#' clu <- signed_blockmodel(tribes,k = 3,alpha=0.5,annealing = TRUE)
#' table(clu$membership)
#' clu$criterion
#' @export
#'

signed_blockmodel <- function(g,k,alpha = 0.5,annealing = FALSE){
  if(!"sign"%in%igraph::edge_attr_names(g)){
    stop("network does not have a sign edge attribute")
  }
  if(missing(k)){
    stop('argument "k" is missing, with no default')
  }
  A <- igraph::get.adjacency(g,"both","sign",sparse = TRUE)
  init_cluster <- sample(0:(k-1),nrow(A),replace = TRUE)
  if(!annealing){
    res <- optimBlocks1(A,init_cluster,k,alpha)
  } else{
    res <- optimBlocksSim(A,init_cluster,k,alpha)
  }
  res$membership <- res$membership+1
  res
}
