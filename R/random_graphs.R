#' @title A graph with random subgraphs conected by negative edges
#' @description Create a number of Erdos-Renyi random graphs with identical parameters, and connect them with the specified number of negative ties.
#' @param islands.n The number of islands in the graph.
#' @param islands.size The size of the islands in the graph.
#' @param islands.pin The probability of intra-island edges.
#' @param n.inter number of negative edges between two islands.
#' @return a signed igraph graph
#' @author David Schoch
#' @examples
#' library(igraph)
#' g <- sample_islands_signed(3, 10, 5/10, 1)
#' @export
sample_islands_signed <- function(islands.n, islands.size, islands.pin, n.inter){
  el <- matrix(0,0,2)
  for(i in 1:islands.n){
    tmp <- t(utils::combn(((i-1)*islands.size+1): (i*islands.size),2))
    tmp <- tmp[sample(c(FALSE,TRUE),nrow(tmp),replace=TRUE,prob = c(1-islands.pin,islands.pin)),]
    el <- rbind(el,tmp)
  }
  el <- cbind(el,1)
  for(i in 1:islands.n){
    outside <- setdiff(1:(islands.size * islands.n),((i-1)*islands.size+1):(i*islands.size))
    inside <- ((i-1)*islands.size+1):(i*islands.size)
    to <- sample(outside,n.inter,replace=TRUE)
    from <- sample(inside,n.inter,replace=TRUE)
    tmp <- cbind(from,to,-1)
    el <- rbind(el,tmp)
  }
  g <- igraph::graph_from_edgelist(el[,1:2],directed=F)
  igraph::E(g)$sign <- el[,3]
  igraph::V(g)$grp <- as.character(rep(1:islands.n,each=islands.size))
  g
}
