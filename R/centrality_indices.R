#' @title PN Centrality Index
#' @description centrality index for signed networks by Everett and Borgatti
#'
#' @param g igraph object. Must have a "sign" edge attribute.
#' @param mode character string, “out” for out-pn, “in” for in-pn or “all” for undirected networks.
#' @return centrality scores as numeric vector.
#' @references Everett, M. and Borgatti, S. (2014) Networks containing negative ties. *Social Networks* 38 111-120
#' @author David Schoch
#' @importFrom Matrix t
#' @export

pn_index <- function(g,mode=c("all","in","out")){
  if (!igraph::is_igraph(g)) {
    stop("Not a graph object")
  }
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

  A <- as_adj_signed(g,sparse = TRUE)
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
#' @param g igraph object. Must have a "sign" edge attribute.
#' @param mode character string, “out” for out-degree, “in” for in-degree or “all” for undirected networks.
#' @param type character string, “pos” or “neg” for counting positive or negative neighbors only,
#' "ratio" for pos/(pos+neg), or "net" for pos-neg.
#' @return centrality scores as numeric vector.
#' @author David Schoch
#' @importFrom Matrix t
#' @export

degree_signed <- function(g,mode=c("all","in","out"), type = c("pos","neg","ratio","net")){
  if (!igraph::is_igraph(g)) {
    stop("Not a graph object")
  }
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

  A <- as_adj_signed(g)
  P <- (A>0)+0
  N <- (A<0)+0

  if(mode=="all"){
    res <- switch(type,
                  pos   = Matrix::rowSums(P),
                  neg   = Matrix::rowSums(N),
                  ratio = Matrix::rowSums(P)/(Matrix::rowSums(P)+Matrix::rowSums(N)),
                  net   = Matrix::rowSums(P)-Matrix::rowSums(N)
    )
    res
  } else if(mode=="out"){
    res <- switch(type,
                  pos   = Matrix::rowSums(P),
                  neg   = Matrix::rowSums(N),
                  ratio = Matrix::rowSums(P)/(Matrix::rowSums(P)+Matrix::rowSums(N)),
                  net   = Matrix::rowSums(P)-Matrix::rowSums(N)
    )
    res
  } else if(mode=="in"){
    res <- switch(type,
                  pos   = Matrix::colSums(P),
                  neg   = Matrix::colSums(N),
                  ratio = Matrix::colSums(P)/(Matrix::colSums(P)+Matrix::colSums(N)),
                  net   = Matrix::colSums(P)-Matrix::colSums(N)
    )
    res
  }
}
