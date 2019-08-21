#' @title PN centrality Index
#' @description centrality index for signed networks by Everett and Borgatti
#'
#' @param g signed network as igraph object
#' @param mode Character string, “out” for out-pn, “in” for in-pn or “all” for undirected networks.
#' @return centrality scores as numeric vector
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

  A <- igraph::get.adjacency(g,type = "both",attr = "sign")
  n <- nrow(A)

  P <- A[A==1]
  N <- A[A==-1]
  I <- diag(1,n)
  A <- P-2*N

  res <- switch(mode,
                all  = solve(I-1/(2*n-2)*A),
                `in` = solve(I-1/(4*(n-1)^2)*t(A)%*%A)%*%solve(I+1/(2*n-2)*t(A)),
                out  = solve(I-1/(4*(n-1)^2)*A%*%t(A))%*%solve(I+1/(2*n-2)*A)
                )
  return(rowSums(res))
}
