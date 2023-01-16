#' @title count signed triangles
#' @description Counts the number of all possible signed triangles (+++),(++-), (+--) and (---)
#'
#' @param g igraph object with a sign edge attribute.
#' @return counts for all 4 signed triangle types
#' @author David Schoch
#' @seealso [signed_triangles]
#' @examples
#' library(igraph)
#' g <- graph.full(4)
#' E(g)$sign <- c(-1, 1, 1, -1, -1, 1)
#' count_signed_triangles(g)
#' @export
count_signed_triangles <- function(g) {
  if (!is_signed(g)) {
    stop("network is not a signed graph")
  }
  if (igraph::is.directed(g)) {
    stop("g must be undirected")
  }
  eattrV <- igraph::get.edge.attribute(g, "sign")

  tmat <- t(matrix(igraph::triangles(g), nrow = 3))
  if (nrow(tmat) == 0) {
    warning("g does not contain any triangles")
    return(c("+++" = 0, "++-" = 0, "+--" = 0, "---" = 0))
  }
  emat <- t(apply(tmat, 1, function(x) {
    c(
      igraph::get.edge.ids(g, x[1:2]),
      igraph::get.edge.ids(g, x[2:3]),
      igraph::get.edge.ids(g, x[c(3, 1)])
    )
  }))


  emat[, 1] <- eattrV[emat[, 1]]
  emat[, 2] <- eattrV[emat[, 2]]
  emat[, 3] <- eattrV[emat[, 3]]
  emat <- t(apply(emat, 1, sort))
  emat_df <- as.data.frame(emat)
  res <- stats::aggregate(list(count = rep(1, nrow(emat_df))), emat_df, length)

  tri_counts <- c("+++" = 0, "++-" = 0, "+--" = 0, "---" = 0)

  tmp_counts <- res[, 4]
  if (nrow(res) == 1) {
    names(tmp_counts) <- paste0(c("+", "-")[(rev(res[1:3]) == -1) + 1], collapse = "")
  } else {
    names(tmp_counts) <- apply(res[, 1:3], 1, function(x) paste0(c("+", "-")[(rev(x) == -1) + 1], collapse = ""))
  }

  tri_counts[match(names(tmp_counts), names(tri_counts))] <- tmp_counts
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
#' g <- graph.full(4)
#' E(g)$sign <- c(-1, 1, 1, -1, -1, 1)
#' signed_triangles(g)
#' @export
signed_triangles <- function(g) {
  if (!is_signed(g)) {
    stop("network is not a signed graph")
  }
  if (igraph::is.directed(g)) {
    stop("g must be undirected")
  }
  eattrV <- igraph::get.edge.attribute(g, "sign")

  tmat <- t(matrix(igraph::triangles(g), nrow = 3))
  if (nrow(tmat) == 0) {
    warning("g does not contain any triangles")
    return(NULL)
  }
  emat <- t(apply(tmat, 1, function(x) {
    c(
      igraph::get.edge.ids(g, x[1:2]),
      igraph::get.edge.ids(g, x[2:3]),
      igraph::get.edge.ids(g, x[c(3, 1)])
    )
  }))

  semat <- matrix(0, nrow(emat), 3)
  semat[, 1] <- eattrV[emat[, 1]]
  semat[, 2] <- eattrV[emat[, 2]]
  semat[, 3] <- eattrV[emat[, 3]]
  cls <- apply(semat, 1, function(v) length(which(v == 1)))
  emat <- cbind(emat, unname(cls))
  colnames(emat) <- c("V1", "V2", "V3", "P")
  emat
}

#' @title count complex triangles
#' @description Counts the number of all possible signed triangles (+++),(++-), (+--) and (---)
#'
#' @param g igraph object.
#' @param attr edge attribute name that encodes positive ("P"), negative ("N") and ambivalent ("A") ties.
#' @return counts for all complex triangle types
#' @author David Schoch
#' @seealso [signed_triangles]
#' @examples
#' library(igraph)
#' g <- graph.full(4)
#' E(g)$type <- c("P", "N", "A", "A", "P", "N")
#' count_complex_triangles(g, attr = "type")
#' @export
count_complex_triangles <- function(g, attr) {
  if (missing(attr)) {
    stop('argument "attr" is missing, with no default')
  }
  if (igraph::is.directed(g)) {
    stop("g must be undirected")
  }
  eattrV <- igraph::get.edge.attribute(g, attr)
  if (!all(eattrV %in% c("P", "N", "A"))) {
    stop('attr may only contain "P","N" and "A" ')
  }

  tmat <- t(matrix(igraph::triangles(g), nrow = 3))
  if (nrow(tmat) == 0) {
    warning("g does not contain any triangles")
    return(c(
      "PPP" = 0, "PPN" = 0, "PNN" = 0, "NNN" = 0,
      "PPA" = 0, "PNA" = 0, "NNA" = 0, "PAA" = 0, "NAA" = 0,
      "AAA" = 0
    ))
  }
  emat <- t(apply(tmat, 1, function(x) {
    c(
      igraph::get.edge.ids(g, x[1:2]),
      igraph::get.edge.ids(g, x[2:3]),
      igraph::get.edge.ids(g, x[c(3, 1)])
    )
  }))


  semat <- matrix(0, nrow(emat), 3)
  semat[, 1] <- eattrV[emat[, 1]]
  semat[, 2] <- eattrV[emat[, 2]]
  semat[, 3] <- eattrV[emat[, 3]]
  semat <- t(apply(semat, 1, sort, decreasing = T))

  emat_df <- as.data.frame(semat)
  res <- stats::aggregate(list(count = rep(1, nrow(emat_df))), emat_df, length)
  tmp_counts <- res[, 4]

  if (nrow(res) == 1) {
    names(tmp_counts) <- paste0(res[1:3], collapse = "")
  } else {
    names(tmp_counts) <- apply(res[, 1:3], 1, function(x) paste0(x, collapse = ""))
  }

  tri_counts <- c(
    "PPP" = 0, "PPN" = 0, "PNN" = 0, "NNN" = 0,
    "PPA" = 0, "PNA" = 0, "NNA" = 0, "PAA" = 0, "NAA" = 0,
    "AAA" = 0
  )

  tri_counts[match(names(tmp_counts), names(tri_counts))] <- tmp_counts
  tri_counts
}

#' @title signed triad census
#' @description triad census for signed graphs
#'
#' @param g igraph object with a sign edge attribute.
#' @return counts for all 139 signed directed triangle types
#' @author David Schoch
#' @examples
#' library(igraph)
#' g <- graph.full(4, directed = TRUE)
#' E(g)$sign <- rep(c(-1, 1, 1, -1, -1, 1),2)
#' triad_census_signed(g)
#' @export

triad_census_signed <- function(g) {
  if (!is_signed(g)) {
    stop("network is not a signed graph")
  }
  if (!igraph::is_directed(g)) {
    stop("g must be a directed graph")
  }
  if (any(igraph::is.multiple(g))) {
    stop("g must be a simple graph")
  }
  tcode <- c(
    "300", "210", "300", "210", "120U", "210", "300", "210", "300",
    "210", "201", "210", "120C", "111U", "120C", "210", "201", "210",
    "300", "210", "300", "210", "120U", "210", "300", "210", "300",
    "210", "120C", "210", "120D", "030T", "120D", "210", "120C",
    "210", "120U", "111U", "120U", "030T", "021D", "030T", "120U",
    "111U", "120U", "210", "120C", "210", "120D", "030T", "120D",
    "210", "120C", "210", "300", "210", "300", "210", "120U", "210",
    "300", "210", "300", "210", "201", "210", "120C", "111U", "120C",
    "210", "201", "210", "300", "210", "300", "210", "120U", "210",
    "300", "210", "300", "210", "120C", "210", "201", "111U", "201",
    "210", "120C", "210", "120D", "111D", "120D", "111D", "102",
    "111D", "120D", "111D", "120D", "210", "120C", "210", "201",
    "111U", "201", "210", "120C", "210", "120C", "030C", "120C",
    "111D", "021C", "111D", "120C", "030C", "120C", "030T", "021C",
    "030T", "021U", "012", "021U", "030T", "021C", "030T", "120C",
    "030C", "120C", "111D", "021C", "111D", "120C", "030C", "120C",
    "210", "120C", "210", "201", "111U", "201", "210", "120C", "210",
    "120D", "111D", "120D", "111D", "102", "111D", "120D", "111D",
    "120D", "210", "120C", "210", "201", "111U", "201", "210", "120C",
    "210", "300", "210", "300", "210", "120U", "210", "300", "210",
    "300", "210", "201", "210", "120C", "111U", "120C", "210", "201",
    "210", "300", "210", "300", "210", "120U", "210", "300", "210",
    "300", "210", "120C", "210", "120D", "030T", "120D", "210", "120C",
    "210", "120U", "111U", "120U", "030T", "021D", "030T", "120U",
    "111U", "120U", "210", "120C", "210", "120D", "030T", "120D",
    "210", "120C", "210", "300", "210", "300", "210", "120U", "210",
    "300", "210", "300", "210", "201", "210", "120C", "111U", "120C",
    "210", "201", "210", "300", "210", "300", "210", "120U", "210",
    "300", "210", "300", "210", "120D", "210", "120C", "030T", "120C",
    "210", "120D", "210", "120C", "111D", "120C", "030C", "021C",
    "030C", "120C", "111D", "120C", "210", "120D", "210", "120C",
    "030T", "120C", "210", "120D", "210", "201", "111D", "201", "111D",
    "021U", "111D", "201", "111D", "201", "111U", "102", "111U",
    "021C", "012", "021C", "111U", "102", "111U", "201", "111D",
    "201", "111D", "021U", "111D", "201", "111D", "201", "210", "120D",
    "210", "120C", "030T", "120C", "210", "120D", "210", "120C",
    "111D", "120C", "030C", "021C", "030C", "120C", "111D", "120C",
    "210", "120D", "210", "120C", "030T", "120C", "210", "120D",
    "210", "120U", "030T", "120U", "111U", "021D", "111U", "120U",
    "030T", "120U", "030T", "021U", "030T", "021C", "012", "021C",
    "030T", "021U", "030T", "120U", "030T", "120U", "111U", "021D",
    "111U", "120U", "030T", "120U", "111U", "021C", "111U", "102",
    "012", "102", "111U", "021C", "111U", "021D", "012", "021D",
    "012", "003", "012", "021D", "012", "021D", "111U", "021C", "111U",
    "102", "012", "102", "111U", "021C", "111U", "120U", "030T",
    "120U", "111U", "021D", "111U", "120U", "030T", "120U", "030T",
    "021U", "030T", "021C", "012", "021C", "030T", "021U", "030T",
    "120U", "030T", "120U", "111U", "021D", "111U", "120U", "030T",
    "120U", "210", "120D", "210", "120C", "030T", "120C", "210",
    "120D", "210", "120C", "111D", "120C", "030C", "021C", "030C",
    "120C", "111D", "120C", "210", "120D", "210", "120C", "030T",
    "120C", "210", "120D", "210", "201", "111D", "201", "111D", "021U",
    "111D", "201", "111D", "201", "111U", "102", "111U", "021C",
    "012", "021C", "111U", "102", "111U", "201", "111D", "201", "111D",
    "021U", "111D", "201", "111D", "201", "210", "120D", "210", "120C",
    "030T", "120C", "210", "120D", "210", "120C", "111D", "120C",
    "030C", "021C", "030C", "120C", "111D", "120C", "210", "120D",
    "210", "120C", "030T", "120C", "210", "120D", "210", "300", "210",
    "300", "210", "120U", "210", "300", "210", "300", "210", "201",
    "210", "120C", "111U", "120C", "210", "201", "210", "300", "210",
    "300", "210", "120U", "210", "300", "210", "300", "210", "120C",
    "210", "120D", "030T", "120D", "210", "120C", "210", "120U",
    "111U", "120U", "030T", "021D", "030T", "120U", "111U", "120U",
    "210", "120C", "210", "120D", "030T", "120D", "210", "120C",
    "210", "300", "210", "300", "210", "120U", "210", "300", "210",
    "300", "210", "201", "210", "120C", "111U", "120C", "210", "201",
    "210", "300", "210", "300", "210", "120U", "210", "300", "210",
    "300", "210", "120C", "210", "201", "111U", "201", "210", "120C",
    "210", "120D", "111D", "120D", "111D", "102", "111D", "120D",
    "111D", "120D", "210", "120C", "210", "201", "111U", "201", "210",
    "120C", "210", "120C", "030C", "120C", "111D", "021C", "111D",
    "120C", "030C", "120C", "030T", "021C", "030T", "021U", "012",
    "021U", "030T", "021C", "030T", "120C", "030C", "120C", "111D",
    "021C", "111D", "120C", "030C", "120C", "210", "120C", "210",
    "201", "111U", "201", "210", "120C", "210", "120D", "111D", "120D",
    "111D", "102", "111D", "120D", "111D", "120D", "210", "120C",
    "210", "201", "111U", "201", "210", "120C", "210", "300", "210",
    "300", "210", "120U", "210", "300", "210", "300", "210", "201",
    "210", "120C", "111U", "120C", "210", "201", "210", "300", "210",
    "300", "210", "120U", "210", "300", "210", "300", "210", "120C",
    "210", "120D", "030T", "120D", "210", "120C", "210", "120U",
    "111U", "120U", "030T", "021D", "030T", "120U", "111U", "120U",
    "210", "120C", "210", "120D", "030T", "120D", "210", "120C",
    "210", "300", "210", "300", "210", "120U", "210", "300", "210",
    "300", "210", "201", "210", "120C", "111U", "120C", "210", "201",
    "210", "300", "210", "300", "210", "120U", "210", "300", "210",
    "300"
  )


  signsign <- c(
    "NNNNNN", "NNN0NN", "NNNNNP", "NNN0NN", "0N0NNN", "NNN0NP",
    "NNNNNP", "NNN0NP", "NNNPNP", "NNN0NN", "00NNNN", "NNP0NN", "0NN0NN",
    "0N00NN", "0NP0NN", "PNN0NN", "00NNPN", "PNP0NN", "NNNNNP", "NNP0NN",
    "NNNNPP", "NNN0PN", "0N0PNN", "NNN0PP", "NNNPPN", "NNP0NP", "NNNPPP",
    "NNN0NN", "0NN0NN", "NNN0PN", "N0N0NN", "0N0NN0", "N0N0NP", "NPN0NN",
    "0NN0NP", "NPN0PN", "0N0NNN", "0N00NN", "0N0PNN", "0N0NN0", "N0N000",
    "0N0PN0", "0N0NNP", "0N00NP", "0N0PNP", "NNN0NP", "0NP0NN", "NNN0PP",
    "N0N0NP", "0N0PN0", "N0N0PP", "NPN0NP", "0NP0NP", "NPN0PP", "NNNNNP",
    "PNN0NN", "NNNPPN", "NPN0NN", "0N0NNP", "NPN0NP", "NPNPNN", "PNN0NP",
    "NPNPNP", "NNN0NP", "00NNPN", "NNP0NP", "0NN0NP", "0N00NP", "0NP0NP",
    "PNN0NP", "00PNPN", "PNP0NP", "NNNPNP", "PNP0NN", "NNNPPP", "NPN0PN",
    "0N0PNP", "NPN0PP", "NPNPNP", "PNP0NP", "NPNPPP", "NNN0NN", "0NN0NN",
    "PNN0NN", "00NNNN", "0N00NN", "00NNPN", "NNP0NN", "0NP0NN", "PNP0NN",
    "N0N0NN", "N000NN", "N0P0NN", "N000NN", "0000NN", "P000NN", "N0P0NN",
    "P000NN", "P0P0NN", "NPN0NN", "0PN0NN", "PPN0NN", "00NNNP", "0P00NN",
    "00NNPP", "NPP0NN", "0PP0NN", "PPP0NN", "0NN0NN", "N00NN0", "0NN0PN",
    "N000NN", "0NN000", "N000NP", "0PN0NN", "N00PN0", "0PN0PN", "0N0NN0",
    "0NN000", "0P0NN0", "0N0N00", "0000N0", "0N0P00", "0N0NP0", "0PN000",
    "0P0NP0", "0NN0NP", "N00PN0", "0NN0PP", "N000PN", "0NP000", "N000PP",
    "0PN0NP", "N00PP0", "0PN0PP", "NNN0PN", "0NN0PN", "PNN0PN", "00NNNP",
    "0N00PN", "00NPPN", "NNP0PN", "0NP0PN", "PNP0PN", "N0N0NP", "N000NP",
    "N0P0NP", "N000PN", "0000NP", "P000PN", "P0N0NP", "P000NP", "P0P0NP",
    "NPN0PN", "0PN0PN", "PPN0PN", "00NPNP", "0P00PN", "00NPPP", "NPP0PN",
    "0PP0PN", "PPP0PN", "NNNNNP", "NNN0PN", "NNNPPN", "NNP0NN", "0N0PNN",
    "NNP0NP", "NNNNPP", "NNN0PP", "NNNPPP", "NPN0NN", "00NNNP", "NPP0NN",
    "0PN0NN", "0P00NN", "0PP0NN", "PPN0NN", "00NNPP", "PPP0NN", "NPNPNN",
    "NNP0PN", "NNPNPP", "NNP0PN", "0P0PNN", "NNP0PP", "NNPNPP", "NNP0PP",
    "NNPPPP", "PNN0NN", "0NN0PN", "PNN0PN", "N0P0NN", "0P0NN0", "N0P0NP",
    "PPN0NN", "0NN0PP", "PPN0PN", "0N0NNP", "0N00PN", "0N0PPN", "0N0NP0",
    "N0P000", "0N0PP0", "0N0NPP", "0N00PP", "0N0PPP", "PNN0NP", "0NP0PN",
    "PNN0PP", "P0N0NP", "0P0PN0", "N0P0PP", "PPN0NP", "0NP0PP", "PPN0PP",
    "NNNPPN", "PNN0PN", "PNNPPN", "NPP0NN", "0N0PPN", "NPP0NP", "NNPNPP",
    "PNN0PP", "PNNPPP", "NPN0NP", "00NPPN", "NPP0NP", "0PN0NP", "0P00NP",
    "0PP0NP", "PPN0NP", "00PNPP", "PPP0NP", "NPNPNP", "PNP0PN", "PNNPPP",
    "NPP0PN", "0P0PNP", "NPP0PP", "PPNPNP", "PNP0PP", "NPPPPP", "NNN0NN",
    "N0N0NN", "NPN0NN", "0NN0NN", "0N0NN0", "0NN0NP", "NNN0PN", "N0N0NP",
    "NPN0PN", "0NN0NN", "N000NN", "0PN0NN", "N00NN0", "0NN000", "N00PN0",
    "0NN0PN", "N000NP", "0PN0PN", "PNN0NN", "N0P0NN", "PPN0NN", "0NN0PN",
    "0P0NN0", "0NN0PP", "PNN0PN", "N0P0NP", "PPN0PN", "00NNNN", "N000NN",
    "00NNNP", "N000NN", "0N0N00", "N000PN", "00NNNP", "N000PN", "00NPNP",
    "0N00NN", "0000NN", "0P00NN", "0NN000", "0000N0", "0NP000", "0N00PN",
    "0000NP", "0P00PN", "00NNPN", "P000NN", "00NNPP", "N000NP", "0N0P00",
    "N000PP", "00NPPN", "P000PN", "00NPPP", "NNP0NN", "N0P0NN", "NPP0NN",
    "0PN0NN", "0N0NP0", "0PN0NP", "NNP0PN", "P0N0NP", "NPP0PN", "0NP0NN",
    "P000NN", "0PP0NN", "N00PN0", "0PN000", "N00PP0", "0NP0PN", "P000NP",
    "0PP0PN", "PNP0NN", "P0P0NN", "PPP0NN", "0PN0PN", "0P0NP0", "0PN0PP",
    "PNP0PN", "P0P0NP", "PPP0PN", "0N0NNN", "0N0NN0", "0N0NNP", "0N00NN",
    "N0N000", "0N00NP", "0N0PNN", "0N0PN0", "0N0PNP", "0N0NN0", "0N0N00",
    "0N0NP0", "0NN000", "0000N0", "0PN000", "0P0NN0", "0N0P00", "0P0NP0",
    "0N0NNP", "0N0NP0", "0N0NPP", "0N00PN", "N0P000", "0N00PP", "0N0PPN",
    "0N0PP0", "0N0PPP", "0N00NN", "0NN000", "0N00PN", "0000NN", "0000N0",
    "0000NP", "0P00NN", "0NP000", "0P00PN", "N0N000", "0000N0", "N0P000",
    "0000N0", "000000", "0000P0", "N0P000", "0000P0", "P0P000", "0N00NP",
    "0PN000", "0N00PP", "0000NP", "0000P0", "0000PP", "0P00NP", "0PP000",
    "0P00PP", "0N0PNN", "0P0NN0", "0N0PPN", "0P00NN", "N0P000", "0P00NP",
    "0P0PNN", "0P0PN0", "0P0PNP", "0N0PN0", "0N0P00", "0N0PP0", "0NP000",
    "0000P0", "0PP000", "0P0PN0", "0P0P00", "0P0PP0", "0N0PNP", "0P0NP0",
    "0N0PPP", "0P00PN", "P0P000", "0P00PP", "0P0PNP", "0P0PP0", "0P0PPP",
    "NNN0NP", "N0N0NP", "NPN0NP", "0NP0NN", "0N0PN0", "0NP0NP", "NNN0PP",
    "N0N0PP", "NPN0PP", "0NN0NP", "N000PN", "0PN0NP", "N00PN0", "0NP000",
    "N00PP0", "0NN0PP", "N000PP", "0PN0PP", "PNN0NP", "P0N0NP", "PPN0NP",
    "0NP0PN", "0P0PN0", "0NP0PP", "PNN0PP", "N0P0PP", "PPN0PP", "00NNPN",
    "N000NP", "00NPPN", "P000NN", "0N0P00", "P000PN", "00NNPP", "N000PP",
    "00NPPP", "0N00NP", "0000NP", "0P00NP", "0PN000", "0000P0", "0PP000",
    "0N00PP", "0000PP", "0P00PP", "00PNPN", "P000NP", "00PNPP", "P000NP",
    "0P0P00", "P000PP", "00PNPP", "P000PP", "00PPPP", "NNP0NP", "N0P0NP",
    "NPP0NP", "0PP0NN", "0N0PP0", "0PP0NP", "NNP0PP", "N0P0PP", "NPP0PP",
    "0NP0NP", "P000PN", "0PP0NP", "N00PP0", "0PP000", "P00PP0", "0NP0PP",
    "P000PP", "0PP0PP", "PNP0NP", "P0P0NP", "PPP0NP", "0PP0PN", "0P0PP0",
    "0PP0PP", "PNP0PP", "P0P0PP", "PPP0PP", "NNNNNP", "NPN0NN", "NPNPNN",
    "PNN0NN", "0N0NNP", "PNN0NP", "NNNPPN", "NPN0NP", "NPNPNP", "NNN0PN",
    "00NNNP", "NNP0PN", "0NN0PN", "0N00PN", "0NP0PN", "PNN0PN", "00NPPN",
    "PNP0PN", "NNNPPN", "NPP0NN", "NNPNPP", "PNN0PN", "0N0PPN", "PNN0PP",
    "PNNPPN", "NPP0NP", "PNNPPP", "NNP0NN", "0PN0NN", "NNP0PN", "N0P0NN",
    "0N0NP0", "P0N0NP", "NPP0NN", "0PN0NP", "NPP0PN", "0N0PNN", "0P00NN",
    "0P0PNN", "0P0NN0", "N0P000", "0P0PN0", "0N0PPN", "0P00NP", "0P0PNP",
    "NNP0NP", "0PP0NN", "NNP0PP", "N0P0NP", "0N0PP0", "N0P0PP", "NPP0NP",
    "0PP0NP", "NPP0PP", "NNNNPP", "PPN0NN", "NNPNPP", "PPN0NN", "0N0NPP",
    "PPN0NP", "NNPNPP", "PPN0NP", "PPNPNP", "NNN0PP", "00NNPP", "NNP0PP",
    "0NN0PP", "0N00PP", "0NP0PP", "PNN0PP", "00PNPP", "PNP0PP", "NNNPPP",
    "PPP0NN", "NNPPPP", "PPN0PN", "0N0PPP", "PPN0PP", "PNNPPP", "PPP0NP",
    "NPPPPP", "NNN0NP", "0NN0NP", "PNN0NP", "00NNPN", "0N00NP", "00PNPN",
    "NNP0NP", "0NP0NP", "PNP0NP", "N0N0NP", "N000PN", "P0N0NP", "N000NP",
    "0000NP", "P000NP", "N0P0NP", "P000PN", "P0P0NP", "NPN0NP", "0PN0NP",
    "PPN0NP", "00NPPN", "0P00NP", "00PNPP", "NPP0NP", "0PP0NP", "PPP0NP",
    "0NP0NN", "N00PN0", "0NP0PN", "P000NN", "0PN000", "P000NP", "0PP0NN",
    "N00PP0", "0PP0PN", "0N0PN0", "0NP000", "0P0PN0", "0N0P00", "0000P0",
    "0P0P00", "0N0PP0", "0PP000", "0P0PP0", "0NP0NP", "N00PP0", "0NP0PP",
    "P000PN", "0PP000", "P000PP", "0PP0NP", "P00PP0", "0PP0PP", "NNN0PP",
    "0NN0PP", "PNN0PP", "00NNPP", "0N00PP", "00PNPP", "NNP0PP", "0NP0PP",
    "PNP0PP", "N0N0PP", "N000PP", "N0P0PP", "N000PP", "0000PP", "P000PP",
    "N0P0PP", "P000PP", "P0P0PP", "NPN0PP", "0PN0PP", "PPN0PP", "00NPPP",
    "0P00PP", "00PPPP", "NPP0PP", "0PP0PP", "PPP0PP", "NNNPNP", "NPN0PN",
    "NPNPNP", "PNP0NN", "0N0PNP", "PNP0NP", "NNNPPP", "NPN0PP", "NPNPPP",
    "NPN0PN", "00NPNP", "NPP0PN", "0PN0PN", "0P00PN", "0PP0PN", "PPN0PN",
    "00NPPP", "PPP0PN", "NPNPNP", "NPP0PN", "PPNPNP", "PNP0PN", "0P0PNP",
    "PNP0PP", "PNNPPP", "NPP0PP", "NPPPPP", "PNP0NN", "0PN0PN", "PNP0PN",
    "P0P0NN", "0P0NP0", "P0P0NP", "PPP0NN", "0PN0PP", "PPP0PN", "0N0PNP",
    "0P00PN", "0P0PNP", "0P0NP0", "P0P000", "0P0PP0", "0N0PPP", "0P00PP",
    "0P0PPP", "PNP0NP", "0PP0PN", "PNP0PP", "P0P0NP", "0P0PP0", "P0P0PP",
    "PPP0NP", "0PP0PP", "PPP0PP", "NNNPPP", "PPN0PN", "PNNPPP", "PPP0NN",
    "0N0PPP", "PPP0NP", "NNPPPP", "PPN0PP", "NPPPPP", "NPN0PP", "00NPPP",
    "NPP0PP", "0PN0PP", "0P00PP", "0PP0PP", "PPN0PP", "00PPPP", "PPP0PP",
    "NPNPPP", "PPP0PN", "NPPPPP", "PPP0PN", "0P0PPP", "PPP0PP", "NPPPPP",
    "PPP0PP", "PPPPPP"
  )
  triple_order <- c(
    "003-000000", "012-0000P0", "012-0000N0", "102-0000PP", "102-0000NP",
    "102-0000NN", "021C-0PP000", "021C-0NP000", "021C-0PN000", "021C-0NN000",
    "021U-0P0P00", "021U-0N0P00", "021U-0N0N00", "021D-P0P000", "021D-N0P000",
    "021D-N0N000", "111U-0P00PP", "111U-0N00PP", "111U-0P00NP", "111U-0P00PN",
    "111U-0N00NP", "111U-0N00PN", "111U-0P00NN", "111U-0N00NN", "111D-P000PP",
    "111D-N000PP", "111D-P000NP", "111D-P000PN", "111D-N000NP", "111D-N000PN",
    "111D-P000NN", "111D-N000NN", "201-00PPPP", "201-00NPPP", "201-00PNPP",
    "201-00NNPP", "201-00NPNP", "201-00NPPN", "201-00PNPN", "201-00NNNP",
    "201-00NNPN", "201-00NNNN", "030C-P00PP0", "030C-N00PP0", "030C-N00PN0",
    "030C-N00NN0", "030T-0P0PP0", "030T-0N0PP0", "030T-0P0NP0", "030T-0P0PN0",
    "030T-0N0NP0", "030T-0N0PN0", "030T-0P0NN0", "030T-0N0NN0", "120U-0P0PPP",
    "120U-0N0PPP", "120U-0P0PNP", "120U-0N0NPP", "120U-0N0PNP", "120U-0N0PPN",
    "120U-0P0PNN", "120U-0N0NNP", "120U-0N0PNN", "120U-0N0NNN", "120D-P0P0PP",
    "120D-N0P0PP", "120D-P0P0NP", "120D-N0N0PP", "120D-N0P0NP", "120D-P0N0NP",
    "120D-P0P0NN", "120D-N0N0NP", "120D-N0P0NN", "120D-N0N0NN", "120C-0PP0PP",
    "120C-0NP0PP", "120C-0PN0PP", "120C-0PP0NP", "120C-0PP0PN", "120C-0NN0PP",
    "120C-0NP0NP", "120C-0NP0PN", "120C-0PN0NP", "120C-0PN0PN", "120C-0PP0NN",
    "120C-0NN0NP", "120C-0NN0PN", "120C-0NP0NN", "120C-0PN0NN", "120C-0NN0NN",
    "210-PPP0PP", "210-NPP0PP", "210-PNP0PP", "210-PPN0PP", "210-PPP0NP",
    "210-PPP0PN", "210-NNP0PP", "210-NPN0PP", "210-NPP0NP", "210-NPP0PN",
    "210-PNN0PP", "210-PNP0NP", "210-PNP0PN", "210-PPN0NP", "210-PPN0PN",
    "210-PPP0NN", "210-NNN0PP", "210-NNP0NP", "210-NNP0PN", "210-NPN0NP",
    "210-NPN0PN", "210-NPP0NN", "210-PNN0NP", "210-PNN0PN", "210-PNP0NN",
    "210-PPN0NN", "210-NNN0NP", "210-NNN0PN", "210-NNP0NN", "210-NPN0NN",
    "210-PNN0NN", "210-NNN0NN", "300-PPPPPP", "300-NPPPPP", "300-NNPPPP",
    "300-NPNPPP", "300-PNNPPP", "300-PPNPNP", "300-NNNPPP", "300-NNPNPP",
    "300-NPNPNP", "300-PNNPPN", "300-NNNNPP", "300-NNNPNP",
    "300-NNNPPN", "300-NPNPNN", "300-NNNNNP", "300-NNNNNN"
  )

  A <- as_adj_signed(g, sparse = TRUE)
  n <- nrow(A)
  adj <- igraph::as_adj_list(igraph::as.undirected(g), "all")
  adj <- lapply(adj, function(x) x - 1)
  triads <- triadCensusSign1(A, adj, n)
  names(triads) <- paste0(tcode, "-", signsign)
  df <- stats::aggregate(count ~ type, data = data.frame(type = names(triads), count = triads), FUN = sum)
  census <- df[["count"]]
  names(census) <- df[["type"]]
  census <- census[match(names(census), triple_order)]
  census[1] <- choose(n, 3) - sum(census)
  census
}
