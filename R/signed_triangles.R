#' @title count signed triangles
#' @description Counts the number of all possible signed triangles (+++),(++-), (+--) and (---)
#'
#' @param g igraph object with a sign edge attribute.
#' @return counts for all 4 signed triangle types
#' @author David Schoch
#' @seealso [signed_triangles]
#' @examples
#' library(igraph)
#' g <- make_full_graph(4)
#' E(g)$sign <- c(-1, 1, 1, -1, -1, 1)
#' count_signed_triangles(g)
#' @export
count_signed_triangles <- function(g) {
  check_signed(g, directed = FALSE)
  tri_counts <- c("+++" = 0, "++-" = 0, "+--" = 0, "---" = 0)
  tri <- triangle_signs(g, "sign")
  if (is.null(tri)) {
    warning("g does not contain any triangles")
    return(tri_counts)
  }
  tri_counts[] <- tabulate(rowSums(tri$signs == -1) + 1, nbins = 4)
  tri_counts
}

#' @title list signed triangles
#' @description lists all possible signed triangles
#'
#' @param g igraph object with a sign edge attribute.
#' @return matrix of vertex ids and the number of positive ties per triangle
#' @author David Schoch
#' @seealso [count_signed_triangles]
#' @examples
#' library(igraph)
#' g <- make_full_graph(4)
#' E(g)$sign <- c(-1, 1, 1, -1, -1, 1)
#' signed_triangles(g)
#' @export
signed_triangles <- function(g) {
  check_signed(g, directed = FALSE)
  tri <- triangle_signs(g, "sign")
  if (is.null(tri)) {
    warning("g does not contain any triangles")
    return(NULL)
  }
  tmat <- cbind(tri$vertices, rowSums(tri$signs == 1))
  colnames(tmat) <- c("V1", "V2", "V3", "P")
  tmat
}

#' @title count complex triangles
#' @description Counts the number of all possible complex triangles, i.e. triangles with positive ("P"), negative ("N") and ambivalent ("A") ties
#'
#' @param g igraph object.
#' @param attr edge attribute name that encodes positive ("P"), negative ("N") and ambivalent ("A") ties.
#' @return counts for all complex triangle types
#' @author David Schoch
#' @seealso [signed_triangles]
#' @examples
#' library(igraph)
#' g <- make_full_graph(4)
#' E(g)$type <- c("P", "N", "A", "A", "P", "N")
#' count_complex_triangles(g, attr = "type")
#' @export
count_complex_triangles <- function(g, attr) {
  if (missing(attr)) {
    stop('argument "attr" is missing, with no default')
  }
  if (igraph::is_directed(g)) {
    stop("g must be undirected")
  }
  if (!attr %in% igraph::edge_attr_names(g)) {
    stop(paste0("There is no edge attribute ", '"', attr, '"'))
  }
  eattrV <- igraph::edge_attr(g, attr)
  if (!all(eattrV %in% c("P", "N", "A"))) {
    stop('attr may only contain "P","N" and "A" ')
  }

  tri_counts <- c(
    "PPP" = 0,
    "PPN" = 0,
    "PNN" = 0,
    "NNN" = 0,
    "PPA" = 0,
    "PNA" = 0,
    "NNA" = 0,
    "PAA" = 0,
    "NAA" = 0,
    "AAA" = 0
  )
  tri <- triangle_signs(g, attr)
  if (is.null(tri)) {
    warning("g does not contain any triangles")
    return(tri_counts)
  }
  # P > N > A, so sorting each row decreasingly gives the canonical name
  nP <- rowSums(tri$signs == "P")
  nN <- rowSums(tri$signs == "N")
  nA <- 3 - nP - nN
  types <- paste0(strrep("P", nP), strrep("N", nN), strrep("A", nA))
  counts <- table(types)
  tri_counts[names(counts)] <- as.vector(counts)
  tri_counts
}

# vertices of all triangles and the attribute values of their three edges
triangle_signs <- function(g, attr) {
  tmat <- matrix(igraph::triangles(g), ncol = 3, byrow = TRUE)
  if (nrow(tmat) == 0) {
    return(NULL)
  }
  pairs <- rbind(tmat[, 1:2], tmat[, 2:3], tmat[, c(3, 1)])
  eids <- igraph::get_edge_ids(g, pairs)
  vals <- igraph::edge_attr(g, attr)[eids]
  list(vertices = tmat, signs = matrix(vals, ncol = 3))
}

#' @title signed triad census
#' @description triad census for signed graphs
#'
#' @param g igraph object with a sign edge attribute.
#' @return counts for all 138 signed directed triangle types
#' @author David Schoch
#' @examples
#' library(igraph)
#' g <- make_full_graph(4, directed = TRUE)
#' E(g)$sign <- rep(c(-1, 1, 1, -1, -1, 1), 2)
#' triad_census_signed(g)
#' @export

triad_census_signed <- function(g) {
  check_signed(g, directed = TRUE)
  if (any(igraph::which_multiple(g))) {
    stop("g must be a simple graph")
  }

  A <- as_adj_signed(g, sparse = TRUE)
  n <- nrow(A)
  adj <- igraph::as_adj_list(igraph::as_undirected(g), "all")
  adj <- lapply(adj, function(x) as.integer(x) - 1L)
  triads <- triadCensusSign1(A, adj, n)
  census <- vapply(
    split(triads, factor(triad_codes, levels = triad_order)),
    sum,
    numeric(1)
  )
  census["003-000000"] <- choose(n, 3) - sum(census[-1])
  census
}
