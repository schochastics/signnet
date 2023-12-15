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
        stop("graph_from_edgelist_sgned expects a matrix with two columns")
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
