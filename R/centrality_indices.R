#' @title PN Centrality Index
#' @description centrality index for signed networks by Everett and Borgatti
#'
#' @param g igraph object. Must have a sign edge attribute.
#' @param mode character string, “out” for out-pn, “in” for in-pn or “all” for undirected networks.
#' @return centrality scores as numeric vector.
#' @references Everett, M. and Borgatti, S. (2014) Networks containing negative ties. *Social Networks* 38 111-120
#' @author David Schoch
#' @importFrom Matrix t
#' @export

pn_index <- function(g,mode=c("all","in","out")){
  if(!"sign"%in%igraph::edge_attr_names(g)){
    stop("network does not have a sign edge attribute")
  }
  mode <- match.arg(mode,c("all","in","out"))
  if(!igraph::is.directed(g)){
    mode <- "all"
  }
  if(igraph::is.directed(g) & mode=="all"){
    stop('"all" only works with undirected networks.')
  }

  A <- igraph::get.adjacency(g,type = "both",attr = "sign")
  n <- nrow(A)

  P <- (A>0)+0
  N <- (A<0)+0
  I <- diag(1,n)
  A <- P-2*N

  res <- switch(mode,
                all  = solve(I-1/(2*n-2)*A),
                `in` = solve(I-1/(4*(n-1)^2)*t(A)%*%A)%*%solve(I+1/(2*n-2)*t(A)),
                out  = solve(I-1/(4*(n-1)^2)*A%*%t(A))%*%solve(I+1/(2*n-2)*A)
                )
  return(rowSums(res))
}

#' @title Signed Degree
#' @description several options to calculate the signed degree of vertices
#'
#' @param g igraph object. Must have a sign edge attribute.
#' @param mode character string, “out” for out-degree, “in” for in-degree or “all” for undirected networks.
#' @param type character string, “pos” or “neg” for counting positive or negative neighbors only,
#' "ratio" for pos/(pos+neg), or "net" for pos-neg.
#' @param ... additional parameters.
#' @return centrality scores as numeric vector.
#' @author David Schoch
#' @importFrom Matrix t
#' @export

degree_signed <- function(g,mode=c("all","in","out"), type = c("pos","neg","ratio","net"),...){
  if(!"sign"%in%igraph::edge_attr_names(g)){
    stop("network does not have a sign edge attribute")
  }
  mode <- match.arg(mode,c("all","in","out"))
  if(!igraph::is.directed(g)){
    mode <- "all"
  }
  if(igraph::is.directed(g) & mode=="all"){
    stop('"all" only works with undirected networks.')
  }
  type <- match.arg(type,c("pos","neg","ratio","net"))

  A <- igraph::as_adj(g,type="both",attr = "sign",...)
  P <- (A>0)+0
  N <- (A<0)+0

  if(mode=="all"){
    res <- switch(type,
                  pos   = rowSums(P),
                  neg   = rowSums(N),
                  ratio = rowSums(P)/(rowSums(P)+rowSums(N)),
                  net   = rowSums(P)-rowSums(N)
    )
    res
  } else if(mode=="out"){
    res <- switch(type,
                  pos   = rowSums(P),
                  neg   = rowSums(N),
                  ratio = rowSums(P)/(rowSums(P)+rowSums(N)),
                  net   = rowSums(P)-rowSums(N)
    )
    res
  } else if(mode=="in"){
    res <- switch(type,
                  pos   = colSums(P),
                  neg   = colSums(N),
                  ratio = colSums(P)/(colSums(P)+colSums(N)),
                  net   = colSums(P)-colSums(N)
    )
    res
  }
}
