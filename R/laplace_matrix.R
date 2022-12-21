#' @title Signed Graph Laplacian
#' @description The Laplacian of a signed graph.
#' @param g igraph object with a sign edge attribute.
#' @param norm Whether to calculate the normalized Laplacian. See definitions below.
#' @param sparse 	Logical scalar, whether to return the result as a sparse matrix. The Matrix package is required for sparse matrices.
#' @details
#' See \link[igraph]{laplacian_matrix} of igraph for more details. In the signed case, D is a diagonal matrix containing the absolute values of row sums of the signed adjacency matrix.
#' @return a numeric matrix
#' @author David Schoch
#' @examples
#' library(igraph)
#' g <- sample_islands_signed(3, 10, 5 / 10, 1)
#' laplacian_matrix_signed(g)
#' laplacian_matrix_signed(g, norm = TRUE)
#' @export

laplacian_matrix_signed <- function(g, norm = FALSE, sparse = FALSE) {
  if (!is_signed(g)) {
    stop("network is not a signed graph")
  }

  A <- as_adj_signed(g, sparse = sparse)
  I <- diag(1, nrow(A))
  D <- diag(rowSums(abs(A)))
  if (norm) {
    diag(D) <- diag(D)^(-1 / 2)
    L <- I - D %*% A %*% D
  } else {
    L <- D - A
  }
  return(L)
}
