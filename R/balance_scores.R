#' @title balancedness of signed network
#' @description Implements several indices to assess the balancedness of a network.
#'
#' @param g signed network.
#' @param method string indicating the method to be used. See details for options
#' @details The method parameter can be one of
#' \describe{
#'   \item{triangles}{Fraction of balanced triangles. Maximal (=1) if all triangles are balanced.}
#'   \item{walk}{\eqn{\sum exp(\lambda_i) / \sum exp(\mu_i)}} where \eqn{\lambda_i} are the eigenvalues of the
#'   signed adjacency matrix and \eqn{\mu_i} of the unsigned adjacency matrix. Maximal (=1) if all walks are balanced.
#' }
#' @return balancedness score
#' @author David Schoch
#' @references
#' Estrada, E. (2019). Rethinking structural balance in signed social networks. *Discrete Applied Mathematics*.
#' @examples
#' library(igraph)
#' g <- graph.full(4)
#' E(g)$sign <- c(-1,1,1,-1,-1,1)
#'
#' balance_score(g, method = "triangles")
#' balance_score(g, method = "walk")
#' @export
balance_score <- function(g,method = "triangles"){
  match.arg(method,c("triangles","walk"))
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
    tria_count <- signed_triangles(g)
    return(unname((tria_count["+++"] + tria_count["+--"])/sum(tria_count)))
  } else if(method == "walk"){
    A <- igraph::get.adjacency(g,attr="sign")
    EigenS <- eigen(A)$values
    EigenU <- eigen(abs(A))$values
    sum(exp(EigenS))/sum(exp(EigenU))
  }
}
