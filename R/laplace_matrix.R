#' @title Signed Graph Laplacian
#' @description The Laplacian of a signed graph.
#' @param g igraph object. Must have a sign edge attribute.
#' @param norm Whether to calculate the normalized Laplacian. See definitions below.
#' @param sparse 	Logical scalar, whether to return the result as a sparse matrix. The Matrix package is required for sparse matrices.
#' @details
#' See \link[igraph]{laplacian_matrix} of igraph for more details. In the signed case, D is a diagonal matrix containing the absolute values of row sums of the signed adjacency matrix.
#' @return a numeric matrix
#' @author David Schoch
#' @examples
#' library(igraph)
#' g <- sample_islands_signed(3, 10, 5/10, 1)
#' laplacian_matrix_signed(g)
#' laplacian_matrix_signed(g,norm = TRUE)
#' @export

laplacian_matrix_signed <- function(g,norm = FALSE,sparse = FALSE){
  if(!"sign"%in%igraph::edge_attr_names(g)){
    stop("network does not have a sign edge attribute")
  }
  A <- igraph::get.adjacency(g,"both",attr = "sign", sparse = sparse)
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


#' @title Angle between Eigenvectors
#' @description Computes the angle between eigenvectors of the signed Laplacian.
#' @param g input graph. Must have a sign edge attribute
#' @param ... additional parameters for Laplacian matrix
#' @details angle between eigenvectors and zero.
#' @return a numeric matrix
#' @author David Schoch
#' @examples
#' library(igraph)
#' g <- sample_islands_signed(3, 10, 5/10, 1)
#' laplacian_angle(g)
#' @export

laplacian_angle <- function(g, ...){
  L <- laplacian_matrix_signed(g,...)
  sL <- eigen(L)
  n <- igraph::vcount(g)

  round(atan2(sL$vectors[,n-1],sL$vectors[,n]),8)
}
