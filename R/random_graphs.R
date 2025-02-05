#' Generate random signed graphs according to the G(n,p) Erdos-Renyi model
#'
#' @param n The number of vertices in the graph.
#' @param p The probability for drawing an edge between two arbitrary vertices.
#' @param p_neg The probability of a drawn edge to be a negative tie
#' @param directed logical, whether the graph will be directed. defaults to FALSE.
#' @param loops logical, whether to add loop edges, defaults to FALSE.
#' @references Erdos, P. and Renyi, A., On random graphs, *Publicationes Mathematicae 6*, 290–297 (1959).
#' @return a signed igraph graph object
#' @export
#'
#' @examples
#' sample_gnp_signed(10, 0.4, 0.5)
sample_gnp_signed <- function(n, p, p_neg, directed = FALSE, loops = FALSE) {
  g <- igraph::sample_gnp(n = n, p = p, directed = directed, loops = loops)
  if (missing(p_neg)) {
    stop("p_neg missing with no default")
  }
  igraph::E(g)$sign <- sample(
    c(-1, 1),
    igraph::ecount(g),
    replace = TRUE,
    prob = c(p_neg, 1 - p_neg)
  )
  g
}

#' Bipartite random signed graphs
#'
#' @param n1 	Integer scalar, the number of bottom vertices.
#' @param n2 	Integer scalar, the number of top vertices.
#' @param p The probability for drawing an edge between two arbitrary vertices.
#' @param p_neg The probability of a drawn edge to be a negative tie
#' @param directed logical, whether the graph will be directed. defaults to FALSE.
#' @param mode Character scalar, specifies how to direct the edges in directed graphs. If it is ‘out’, then directed edges point from bottom vertices to top vertices. If it is ‘in’, edges point from top vertices to bottom vertices. ‘out’ and ‘in’ do not generate mutual edges. If this argument is ‘all’, then each edge direction is considered independently and mutual edges might be generated. This argument is ignored for undirected graphs.
#'
#' @return A signed bipartite igraph graph.
#' @export
#'
#' @examples
#' sample_bipartite_signed(10, 10, 0.5, 0.5)
sample_bipartite_signed <- function(
  n1,
  n2,
  p,
  p_neg,
  directed = FALSE,
  mode = c("out", "in", "all")
) {
  g <- igraph::sample_bipartite(
    n1 = n1,
    n2 = n2,
    p = p,
    directed = directed,
    mode = mode
  )
  if (missing(p_neg)) {
    stop("p_neg missing with no default")
  }
  igraph::E(g)$sign <- sample(
    c(-1, 1),
    igraph::ecount(g),
    replace = TRUE,
    prob = c(p_neg, 1 - p_neg)
  )
  g
}

#' @title A graph with random subgraphs connected by negative edges
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
sample_islands_signed <- function(
  islands.n,
  islands.size,
  islands.pin,
  n.inter
) {
  if (islands.n <= 1) {
    stop("islands.n should be greater than one")
  }
  if (islands.size <= 1) {
    stop("islands.size should be greater than one")
  }
  if (islands.pin < 0 || islands.pin > 1) {
    stop("islands.pin should be between zero and one")
  }
  if (n.inter <= 0) {
    stop("n.inter should be greater than zero")
  }
  el <- matrix(0, 0, 2)
  for (i in 1:islands.n) {
    tmp <- t(utils::combn(((i - 1) * islands.size + 1):(i * islands.size), 2))
    tmp <- tmp[
      sample(
        c(FALSE, TRUE),
        nrow(tmp),
        replace = TRUE,
        prob = c(1 - islands.pin, islands.pin)
      ),
    ]
    el <- rbind(el, tmp)
  }
  el <- cbind(el, 1)
  for (i in 1:islands.n) {
    outside <- setdiff(
      1:(islands.size * islands.n),
      ((i - 1) * islands.size + 1):(i * islands.size)
    )
    inside <- ((i - 1) * islands.size + 1):(i * islands.size)
    to <- sample(outside, n.inter, replace = TRUE)
    from <- sample(inside, n.inter, replace = TRUE)
    tmp <- cbind(from, to, -1)
    el <- rbind(el, tmp)
  }
  g <- igraph::graph_from_edgelist(el[, 1:2], directed = FALSE)
  igraph::E(g)$sign <- el[, 3]
  igraph::V(g)$grp <- as.character(rep(1:islands.n, each = islands.size))
  g <- igraph::delete_edges(g, which(igraph::which_multiple(g)))
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
graph_circular_signed <- function(n, r = 1, pos = 0.1, neg = 0.1) {
  if (missing(n)) {
    stop('argument "n" is missing, with no default')
  }
  pts <- circleFun(r = r, npoints = n)

  D <- arcDistMat(as.matrix(pts), r)

  thr <- (2 * pi * r) * pos
  anti <- arc_dist(c(0, r), c(0, -r), r) * (1 - neg)
  P <- (D <= thr & D != 0) + 0
  N <- (D >= anti & D != 0) + 0

  A <- P - N
  g <- igraph::graph_from_adjacency_matrix(
    A,
    mode = "undirected",
    weighted = TRUE
  )
  igraph::E(g)$sign <- ifelse(igraph::E(g)$weight == 1, 1, -1)
  g <- igraph::delete_edge_attr(g, "weight")
  igraph::V(g)$x <- pts$x
  igraph::V(g)$y <- pts$y
  g
}

# create points on a circle with radius r around center
circleFun <- function(center = c(0, 0), r = 1, npoints = 20) {
  pts_seq <- seq(0, 2 * pi, length.out = npoints * 100)
  pts_samp <- sample(pts_seq, npoints)

  xx <- center[1] + r * cos(pts_samp)
  yy <- center[2] + r * sin(pts_samp)
  return(data.frame(x = xx, y = yy))
}

# distance between two points x and y on a circle with radius r
arc_dist <- function(x, y, r) {
  c <- sqrt((x[1] - y[1])^2 + (x[2] - y[2])^2)
  theta <- acos((2 * r^2 - c^2) / (2 * r^2))
  2 * pi * r * theta / (2 * pi)
}
