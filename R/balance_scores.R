#' @title balancedness of signed network
#' @description Implements several indices to assess the balancedness of a network.
#'
#' @param g signed network.
#' @param method string indicating the method to be used. See details for options
#' @details The method parameter can be one of
#' \describe{
#'   \item{*triangles*}{Fraction of balanced triangles. Maximal (=1) if all triangles are balanced.}
#'   \item{*walk*}{\eqn{\sum exp(\lambda_i) / \sum exp(\mu_i)}} where \eqn{\lambda_i} are the eigenvalues of the
#'   signed adjacency matrix and \eqn{\mu_i} of the unsigned adjacency matrix. Maximal (=1) if all walks are balanced.
#'   \item{*frustration*}{The frustration index assumes that the network can be partitioned into two groups, where intra group edges are positive and inter group edges are negative. The index is defined as the sum of intra group negative and inter group positive edges. Note that the problem is NP complete and only an upper bound is returned (based on simulated annealing). Exact methods can be found in the work of Aref. The index is normalized such that it is maximal (=1) if the network is balanced.}
#' }
#' @return balancedness score
#' @author David Schoch
#' @references
#' Estrada, E. (2019). Rethinking structural balance in signed social networks. *Discrete Applied Mathematics*.
#'
#' Samin Aref, Mark C Wilson (2018). Measuring partial balance in signed networks. *Journal of Complex Networks*, 6(4): 566–595, https://doi.org/10.1093/comnet/cnx044
#' @examples
#' library(igraph)
#' g <- graph.full(4)
#' E(g)$sign <- c(-1,1,1,-1,-1,1)
#'
#' balance_score(g, method = "triangles")
#' balance_score(g, method = "walk")
#' @export
balance_score <- function(g,method = "triangles"){
  method <- match.arg(method,c("triangles","walk","frustration"))
  if(!"sign"%in%igraph::edge_attr_names(g)){
    stop("network does not have a sign edge attribute")
  }
  if(igraph::is.directed(g)){
    stop("g must be undirected")
  }
  eattrV <- igraph::get.edge.attribute(g,"sign")
  if(!all(eattrV%in%c(-1,1))){
    stop("sign may only contain -1 and 1")
  }
  if(method == "triangles"){
    tria_count <- count_signed_triangles(g)
    return(unname((tria_count["+++"] + tria_count["+--"])/sum(tria_count)))
  } else if(method == "walk"){
    A <- as_adj_signed(g,sparse = TRUE)
    EigenS <- eigen(A)$values
    EigenU <- eigen(abs(A))$values
    return(sum(exp(EigenS))/sum(exp(EigenU)))
  } else if(method == "frustration"){
    clu <- signed_blockmodel(g,k = 2,alpha = 0.5,annealing = TRUE)
    return(1-clu$criterion/(igraph::ecount(g)/2))
  }
}
