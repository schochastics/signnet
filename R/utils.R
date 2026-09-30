#' Check if network is a signed network
#'
#' @param g igraph object
#'
#' @return logical scalar
#' @export
#'
#' @examples
#' g <- sample_islands_signed(2, 5, 1, 5)
#' is_signed(g)
is_signed <- function(g) {
  if (!igraph::is_igraph(g)) {
    stop("Not a graph object")
  }
  if (!"sign" %in% igraph::edge_attr_names(g)) {
    return(FALSE)
  }
  eattrV <- igraph::edge_attr(g, "sign")
  if (!all(eattrV %in% c(-1, 1))) {
    return(FALSE)
  }
  return(TRUE)
}

#' Create signed graphs from adjacency matrices
#'
#' @param A square adjacency matrix of a signed graph
#' @param mode Character scalar, specifies how to interpret the supplied matrix.  Possible values are: directed, undirected
#' @param ... additional parameters for `from_adjacency()`
#'
#' @return a signed network as igraph object
#' @export
#'
#' @examples
#' A <- matrix(c(0, 1, -1, 1, 0, 1, -1, 1, 0), 3, 3)
#' graph_from_adjacency_matrix_signed(A)
graph_from_adjacency_matrix_signed <- function(A, mode = "undirected", ...) {
  if (!all(A %in% c(-1, 0, 1))) {
    stop("A should only have entries -1,0,1")
  }
  igraph::graph_from_adjacency_matrix(A, mode = mode, weighted = "sign", ...)
}

#' Create a signed graph from an edgelist matrix
#'
#' @param el The edgelist, a two column matrix, character or numeric.
#' @param signs vector indicating the sign of edges. Entries must be 1 or -1.
#' @param directed whether to create a directed graph.
#'
#' @return a signed network as igraph object
#' @export
#'
#' @examples
#' el <- matrix(c("foo", "bar", "bar", "foobar"), ncol = 2, byrow = TRUE)
#' signs <- c(-1, 1)
#' graph_from_edgelist_signed(el, signs)
graph_from_edgelist_signed <- function(el, signs, directed = FALSE) {
  if (!is.matrix(el) || ncol(el) != 2) {
    stop("graph_from_edgelist_signed expects a matrix with two columns")
  }
  if (length(signs) != nrow(el)) {
    stop("signs and el must have the same length")
  }
  if (!all(signs %in% c(-1, 1))) {
    stop("signs should only have entries -1 or 1")
  }
  g <- igraph::graph_from_edgelist(el, directed = directed)
  igraph::E(g)$sign <- signs
  g
}

# input validation ----

check_graph <- function(g) {
  if (!igraph::is_igraph(g)) {
    stop("Not a graph object", call. = FALSE)
  }
}

check_sign_attr <- function(g) {
  check_graph(g)
  if (!"sign" %in% igraph::edge_attr_names(g)) {
    stop("network does not have a sign edge attribute", call. = FALSE)
  }
}

# directed = NULL: any, FALSE: must be undirected, TRUE: must be directed
check_signed <- function(g, directed = NULL) {
  if (!is_signed(g)) {
    stop("network is not a signed graph", call. = FALSE)
  }
  if (isFALSE(directed) && igraph::is_directed(g)) {
    stop("g must be undirected", call. = FALSE)
  }
  if (isTRUE(directed) && !igraph::is_directed(g)) {
    stop("g must be a directed graph", call. = FALSE)
  }
}

# matrix construction ----

# sparse signed adjacency matrix; values of multiple edges are summed and
# undirected loops are counted once
signed_adjacency <- function(g) {
  n <- igraph::vcount(g)
  el <- igraph::as_edgelist(g, names = FALSE)
  x <- as.numeric(igraph::edge_attr(g, "sign"))
  i <- el[, 1]
  j <- el[, 2]
  if (!igraph::is_directed(g)) {
    offdiag <- i != j
    i <- c(i, el[offdiag, 2])
    j <- c(j, el[offdiag, 1])
    x <- c(x, x[offdiag])
  }
  vnames <- igraph::V(g)$name
  Matrix::drop0(Matrix::sparseMatrix(
    i = i,
    j = j,
    x = x,
    dims = c(n, n),
    dimnames = if (is.null(vnames)) NULL else list(vnames, vnames)
  ))
}

# sparse signed incidence matrix of a two-mode network: rows are the vertices
# with type FALSE, columns the vertices with type TRUE
signed_biadjacency <- function(g) {
  types <- as.logical(igraph::V(g)$type)
  el <- igraph::as_edgelist(g, names = FALSE)
  if (any(types[el[, 1]] == types[el[, 2]])) {
    stop("edges must connect vertices of different types", call. = FALSE)
  }
  rows <- which(!types)
  cols <- which(types)
  from_row <- !types[el[, 1]]
  r <- ifelse(from_row, el[, 1], el[, 2])
  c <- ifelse(from_row, el[, 2], el[, 1])
  vnames <- igraph::V(g)$name
  if (is.null(vnames)) {
    vnames <- as.character(seq_along(types))
  }
  Matrix::drop0(Matrix::sparseMatrix(
    i = match(r, rows),
    j = match(c, cols),
    x = as.numeric(igraph::edge_attr(g, "sign")),
    dims = c(length(rows), length(cols)),
    dimnames = list(vnames[rows], vnames[cols])
  ))
}
