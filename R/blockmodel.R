#' @title Blockmodelling for signed networks
#' @description Finds blocks of nodes with intra-positive and inter-negative edges
#' @param g igraph object. Must have a "sign" edge attribute.
#' @param k number of blocks
#' @param alpha see details
#' @param annealing logical. if TRUE, use simulated annealing (Default: FALSE)
#' @return numeric vector of block assignments and the associated criterion value
#' @details The function minimizes P(C)=\eqn{\alpha}N+(1-\eqn{\alpha})P,
#' where N is the total number of negative ties within plus-sets and P be the total number of
#' positive ties between plus-sets. This function implementes the structural balance model. That is,
#' all diagonal blocks are positive and off-diagonal blocks negative.
#' For the generalized version see [signed_blockmodel_general].
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


#' @title Generalized blockmodelling for signed networks
#' @description Finds blocks of nodes with specified inter/intra group ties
#' @param g igraph object. Must have a "sign" edge attribute.
#' @param blockmat Integer Matrix. Specifies the inter/intra group patterns of ties
#' @param alpha see details
#' @return numeric vector of block assignments and the associated criterion value
#' @details The function minimizes P(C)=\eqn{\alpha}N+(1-\eqn{\alpha})P,
#' where N is the total number of negative ties within plus-sets and P be the total number of
#' positive ties between plus-sets. This function implementes the generalized model. For the structural balance
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
#' g <- Reduce("%u%",list(g1,g2,g3))
#' E(g)$sign <- 1
#' E(g)$sign[1:10] <- -1
#' g <- add.edges(g,c(rbind(1:5,6:10)),attr = list(sign=-1))
#' g <- add.edges(g,c(rbind(1:5,11:15)),attr = list(sign=-1))
#' g <- add.edges(g,c(rbind(11:15,6:10)),attr = list(sign=1))
#'
#' # specify the link patterns between groups
#' blockmat <- matrix(c(1,-1,-1,-1,1,1,-1,1,-1),3,3,byrow = TRUE)
#' res <- signed_blockmodel_general(g,blockmat,0.5)
#' res$membership
#' res$criterion
#' @export
#'

signed_blockmodel_general <- function(g,blockmat,alpha = 0.5){
  if(!"sign"%in%igraph::edge_attr_names(g)){
    stop("network does not have a sign edge attribute")
  }
  if(missing(blockmat)){
    stop('argument "blockmat" is missing, with no default')
  }
  if(!all(blockmat%in%c(-1,1))){
    stop('"blockmat" may only contain -1 and 1')
  }
  A <- igraph::get.adjacency(g,"both","sign",sparse = TRUE)
  init_cluster <- sample(0:(nrow(blockmat)-1),nrow(A),replace = TRUE)
  res <- optimBlocksSimS(A,init_cluster,blockmat,alpha)
  res$membership <- res$membership+1
  res
}
