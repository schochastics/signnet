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
#' sample_islands_signed(3, 10, 0.5, 1)
#' @export
sample_islands_signed <- function(islands.n, islands.size, islands.pin, n.inter){
  if(islands.n<=1){
    stop("islands.n should be greater than one")
  }
  if(islands.size<=1){
    stop("islands.size should be greater than one")
  }
  if(islands.pin<0|islands.pin>1){
    stop("islands.pin should be between zero and one")
  }
  if(n.inter<=0){
    stop("n.inter should be greater than zero")
  }
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
  g <- igraph::delete.edges(g,which(igraph::is.multiple(g)))
  g
}

#' @title circular signed graph
#' @description  circular graph with positive and negative edges.
#'
#' @param n number of nodes
#' @param r radius
#' @param pos distance fraction between positive edges
#' @param neg distance fraction between negative edges
#' @return igraph graph
#' @author David Schoch
#' @examples
#' library(igraph)
#' graph_circular_signed(n = 50)
#' @export
#
graph_circular_signed <- function(n,r = 1,pos = 0.1,neg = 0.1){
  if(missing(n)){
    stop('argument "n" is missing, with no default')
  }
  pts <- circleFun(r=r,npoints=n)

  D <- arcDistMat(as.matrix(pts),r)

  thr <- (2*pi*r)*pos
  anti <- arc_dist(c(0,r),c(0,-r),r)*(1-neg)
  P <- (D<=thr & D!=0)+0
  N <- (D>=anti & D!=0)+0

  A <- P-N
  g <- igraph::graph_from_adjacency_matrix(A,mode="undirected",weighted = T)
  igraph::E(g)$sign <- ifelse(igraph::E(g)$weight==1,1,-1)
  g <- igraph::delete_edge_attr(g,"weight")
  igraph::V(g)$x <- pts$x
  igraph::V(g)$y <- pts$y
  g
}

#helper
circleFun <- function(center = c(0,0),r = 1, npoints = 20){
  pts_seq <- seq(0,2*pi,length.out = npoints*100)
  pts_samp <- sample(pts_seq,npoints)

  xx <- center[1] + r * cos(pts_samp)
  yy <- center[2] + r * sin(pts_samp)
  return(data.frame(x = xx, y = yy))
}

arc_dist <- function(x,y,r){
  c <- sqrt((x[1]-y[1])^2+(x[2]-y[2])^2)
  theta <- acos((2*r^2-c^2)/(2*r^2))
  2*pi*r*theta/(2*pi)
}
