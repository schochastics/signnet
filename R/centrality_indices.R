#' @title PN Centrality Index
#' @description centrality index for signed networks by Everett and Borgatti
#'
#' @param g igraph object with a sign edge attribute.
#' @param mode character string, “out” for out-pn, “in” for in-pn or “all” for undirected networks.
#' @return centrality scores as numeric vector.
#' @references Everett, M. and Borgatti, S. (2014) Networks containing negative ties. *Social Networks* 38 111-120
#' @author David Schoch
#' @importFrom Matrix t
#' @examples
#' A <- matrix(c(0,  1,  0,  1,  0,  0,  0, -1, -1,  0,
#'               1,  0,  1, -1,  1, -1, -1,  0,  0,  0,
#'               0,  1,  0,  1, -1,  0,  0,  0, -1,  0,
#'               1, -1,  1,  0,  1, -1, -1,  0,  0,  0,
#'               0,  1, -1,  1,  0,  1,  0, -1,  0, -1,
#'               0, -1,  0, -1,  1,  0,  1,  0,  1, -1,
#'               0, -1,  0, -1,  0,  1,  0,  1, -1,  1,
#'              -1,  0,  0,  0, -1,  0,  1,  0,  1,  0,
#'              -1,  0, -1,  0,  0,  1, -1,  1,  0,  1,
#'               0,  0,  0,  0, -1, -1,  1,  0,  1,  0), 10, 10)

#'g <- graph_from_adjacency_matrix_signed(A,"undirected")
#'pn_index(g)
#' @export

pn_index <- function(g,mode=c("all","in","out")){
  if (!is_signed(g)) {
    stop("network is not a signed graph")
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
                `in` = solve(I-1/(4*(n-1)^2)*Matrix::t(A)%*%A)%*%(I+1/(2*n-2)*Matrix::t(A)),
                out  = solve(I-1/(4*(n-1)^2)*A%*%Matrix::t(A))%*%(I+1/(2*n-2)*A)
  )
  return(Matrix::rowSums(res))
}

#' @title Signed Degree
#' @description several options to calculate the signed degree of vertices
#'
#' @param g igraph object with a sign edge attribute.
#' @param mode character string, “out” for out-degree, “in” for in-degree or “all” for undirected networks.
#' @param type character string, “pos” or “neg” for counting positive or negative neighbors only,
#' "ratio" for pos/(pos+neg), or "net" for pos-neg.
#' @return centrality scores as numeric vector.
#' @author David Schoch
#' @importFrom Matrix t
#' @export

degree_signed <- function(g,mode=c("all","in","out"), type = c("pos","neg","ratio","net")){
  if (!is_signed(g)) {
    stop("network is not a signed graph")
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
    return(res)
  }
}

#' @title Signed Eigenvector centrality
#' @description returns the eigenvector associated with the dominant eigenvalue from the adjacency matrix.
#' @details Note that, with negative values, the adjacency matrix may not have a dominant eigenvalue.
#' This means it is not clear which eigenvector should be used. In addition it is possible for the adjacency matrix to have repeated eigenvalues and hence multiple linearly independent eigenvectors. In this case certain centralities can be arbitrarily assigned. The function returns an error if this is the case.
#' @param g igraph object with a sign edge attribute.
#' @param scale Logical scalar, whether to scale the result to have a maximum score of one. If no scaling is used then the result vector is the same as returned by `eigen()`.
#' @return centrality scores as numeric vector.
#' @references
#' Bonacich, P. and Lloyd, P. (2004). "Calculating Status with Negative Relations." *Social Networks* 26 (4): 331–38.
#'
#' Everett, M. and Borgatti, S.P. (2014). "Networks Containing Negative Ties." *Social Networks* 38: 111–20.
#'
#' @author David Schoch
#' @examples
#' library(igraph)
#' data("tribes")
#' eigen_centrality_signed(tribes)
#' @export

eigen_centrality_signed <- function(g, scale = TRUE){
  if (!is_signed(g)) {
    stop("network is not a signed graph")
  }

  sA <- eigen(as_adj_signed(g))
  evals <- round(sA$values,8)
  max_evals <- which(abs(evals)==max(abs(evals)))

  if(length(max_evals)!=1){
    stop("no dominant eigenvalue exists")
  } else{
    evcent <- abs(sA$vectors[,max_evals])
  }

  if(scale) evcent <- evcent/max(evcent)

  return(evcent)
}
