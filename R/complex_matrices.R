#' Convert a signed graph to a complex adjacency matrix
#'
#' This function returns the adjacency matrix for a signed graph that contains ambivalent ties
#'
#' @param g igraph object
#' @param attr edge attribute name that encodes positve ("P"), negative ("N") and ambivalent ("A") ties.
#' @return complex adjacency matrix
#' @export

as_complex_adj <- function(g,attr){
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
  A <- as_complex_adj(g,attr)
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
