#' Convert a signed graph to a signed adjacency matrix
#'
#' This function returns the adjacency matrix for a signed graph
#'
#' @param g igraph object. Must have a "sign" edge attribute.
#' @param sparse 	Logical scalar, whether to return the result as a sparse matrix. The Matrix package is required for sparse matrices.
#' @return signed adjacency matrix
#' @seealso [as_adj_complex]
#' @export

as_adj_signed <- function(g,sparse = FALSE){
  if (!igraph::is_igraph(g)) {
    stop("Not a graph object")
  }
  if(!"sign"%in%igraph::edge_attr_names(g)){
    stop("network does not have a sign edge attribute")
  }
  igraph::as_adj(g,type = "both",attr = "sign", sparse = sparse)
}


#' Convert a signed graph to a complex adjacency matrix
#'
#' This function returns the adjacency matrix for a signed graph that contains ambivalent ties
#'
#' @param g igraph object
#' @param attr edge attribute name that encodes positve ("P"), negative ("N") and ambivalent ("A") ties.
#' @return complex adjacency matrix
#' @seealso [as_adj_signed]
#' @export

as_adj_complex <- function(g,attr){
  if (!igraph::is_igraph(g)) {
    stop("Not a graph object")
  }
  if(igraph::is.directed(g)){
    stop("directed graphs are not supported")
  }
  if(missing(attr)){
    stop('argument "attr" is missing, with no default')
  }

  eattr <- igraph::get.edge.attribute(g,attr)
  if(!all(eattr%in%c("P","N","A"))){
    stop('attr may only contain "P","N" and "A" ')
  }
  A <- igraph::as_adj(g,type = "both",attr,sparse=FALSE)
  A <- replace(A,A=="P",complex(1,1,0))
  A <- replace(A,A=="N",complex(1,0,1))
  A <- replace(A,A=="A",complex(1,0.5,0.5))
  A <- replace(A,A=="",complex(1,0,0))
  class(A) <- "complex"
  A[lower.tri(A)] <- Conj(A[lower.tri(A)])
  A
}


#' @title Complex Graph Laplacian
#' @description The Laplacian of a signed graph containing ambivalent ties.
#' @param g igraph object.
#' @param attr edge attribute name that encodes positve ("P"), negative ("N") and ambivalent ("A") ties.
#' @param norm Whether to calculate the normalized Laplacian. See definitions below.
#' @details
#' See \link[igraph]{laplacian_matrix} of igraph for more details. In the complex case, D is a diagonal matrix containing the absolute values of row sums of the complex adjacency matrix.
#' @return a complex matrix
#' @seealso [laplacian_matrix_signed]
#' @author David Schoch
#' @export

laplacian_matrix_complex <- function(g,attr,norm = FALSE){
  if (!igraph::is_igraph(g)) {
    stop("Not a graph object")
  }
  if(igraph::is.directed(g)){
    stop("directed graphs are not supported")
  }
  A <- as_adj_complex(g,attr)
  I <- diag(1,nrow(A))
  D <- diag(rowSums(abs(A)))
  if(norm){
    diag(D) <- diag(D)^(-1/2)
    L <- I-D%*%A%*%D
  } else{
    L <- D-A
  }
  return(L)
}

#' @title Complex Incidence Matrix
#' @description The complex incidence matrix of a signed graph containing ambivalent ties.
#' @param g igraph object.
#' @param attr edge attribute name that encodes positve ("P"), negative ("N") and ambivalent ("A") ties.
#' @details
#' This function is slightly different than \link[igraph]{as_incidence_matrix} since it is defined for bipartite graphs.
#' The incidence matrix here is defined as a \eqn{S \in C^{n,m}}, where n is the number of vertices and m the number of edges. Edges (i,j) are oriented such that i<j and entries are defined as
#' \deqn{S_{i(i,j)}=\sqrt{A_{ij}}}
#' \deqn{S_{j(i,j)}=-\sqrt{A_{ji}} if (i,j) is an ambivalent tie}
#' \deqn{S_{j(i,j)}=-A_{ji}\sqrt{A_{ji}} else}
#' @return a complex matrix
#' @seealso [laplacian_matrix_complex],[as_adj_complex]
#' @author David Schoch
#' @export
as_incidence_complex <- function(g,attr){
  A <- as_adj_complex(g,attr)
  N <- igraph::vcount(g)
  M <- igraph::ecount(g)
  S <- matrix(0,N,M)
  e <- 0
  for(i in 1:N){
    for(j in i:N){
      if(A[i,j]!=0){
        e <- e+1
        if(A[i,j]==complex(1,1,0)){
          S[i,e] <- sqrt(A[i,j])
          S[j,e] <- -A[j,i]*sqrt(A[i,j])
        } else if(A[i,j]==complex(1,0,1)){
          S[i,e] <- sqrt(A[i,j])
          S[j,e] <- -A[j,i]*sqrt(A[i,j])
        }
        else {
          S[i,e] <- sqrt(A[i,j])
          S[j,e] <- -sqrt(A[j,i])
        }
      }
    }
  }
  S
}
