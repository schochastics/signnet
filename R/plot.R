#' @title Plot Blockmodel matrix
#' @param g igraph object with a sign edge attribute.
#' @param blocks vector of block membership as obtained, e.g. from [signed_blockmodel]
#' @param cols colors used for negative and positive ties
#' @param show_blocks logical. Should block borders be displayed? (Default: FALSE)
#' @param show_labels logical. Should node labels be displayed? (Default: FALSE)
#' @return ggplot2 object
#' @author David Schoch
#' @examples
#' \dontrun{
#' library(igraph)
#' data("tribes")
#' clu <- signed_blockmodel(tribes, k = 3, alpha = 0.5, annealing = TRUE)
#' ggblock(tribes, clu$membership, show_blocks = TRUE, show_labels = TRUE)
#' }
#' @export
#'
ggblock <- function(g, blocks = NULL, cols = NULL, show_blocks = FALSE, show_labels = FALSE) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The package 'ggplot2' is needed for this function.")
  }
  if (is.null(cols)) {
    cols <- c("firebrick", "steelblue")
  }
  if (is.null(blocks)) {
    permI <- 1:igraph::vcount(g)
    blocks <- rep(1, igraph::vcount(g))
  } else {
    permI <- order(blocks)
  }
  if (show_blocks) {
    bsizes <- unname(table(blocks))
    bsizes <- cumsum(bsizes) + 0.5
    rsizes <- igraph::vcount(g) - bsizes[-length(bsizes)] + 1
    csizes <- bsizes[-length(bsizes)]
  }
  if (!"name" %in% igraph::vertex_attr_names(g)) {
    g <- igraph::set_vertex_attr(g, "name", value = 1:igraph::vcount(g))
  }
  A <- as.matrix(igraph::as_adj(g, type = "both", attr = "sign"))
  df <- data.frame(from = rep(rownames(A), ncol(A)), to = rep(colnames(A), each = nrow(A)), value = c(A))
  df[["from"]] <- factor(df[["from"]], levels = rev(rownames(A)[permI]))
  df[["to"]] <- factor(df[["to"]], levels = colnames(A)[permI])
  df <- df[df[["value"]] != 0, ]
  df[["value"]] <- as.factor(df[["value"]])
  p <- ggplot2::ggplot(df, ggplot2::aes_(y = ~from, x = ~to)) +
    ggplot2::geom_tile(ggplot2::aes_(fill = ~value), col = "white") +
    ggplot2::scale_fill_manual(values = cols) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_fixed()
  if (show_blocks) {
    p <- p + ggplot2::geom_vline(xintercept = csizes) + ggplot2::geom_hline(yintercept = rsizes)
  }
  if (show_labels) {
    p <- p + ggplot2::scale_x_discrete(position = "top") +
      ggplot2::theme(axis.text.y = ggplot2::element_text())
  }
  p
}

#' @title Plot a signed or complex network
#' @param g igraph object. Must have a "sign" edge attribute or an attribute containing "P", "N", "A"
#' @param type character string. either "signed" or "complex"
#' @param attr character string. edge attribute that containing "P", "N", "A" if type="complex"
#' @param edge_cols colors used for negative and positive (and ambivalent) ties
#' @param weights logical. If TRUE, weights are computed based on sign. Defaults to FALSE
#' @details This is a very rudimentary visualization of a signed network. If you are fluent in 'ggraph', you can probably cook up something more sophisticated. The function is thus mostly meant to give a quick overview of the network.
#' @return ggplot2 object
#' @author David Schoch
#' @export
ggsigned <- function(g, type = "signed", attr = NULL, edge_cols = NULL, weights = FALSE) {
  if (!requireNamespace("ggraph", quietly = TRUE)) {
    stop("The package 'ggraph' is needed for this function.")
  }
  type <- match.arg(type, c("signed", "complex"))
  if (is.null(edge_cols) & type == "signed") {
    edge_cols <- c(`-1` = "firebrick", `1` = "steelblue")
  }
  if (is.null(edge_cols) & type == "complex") {
    edge_cols <- c(`N` = "firebrick", `P` = "steelblue", `A` = "darkorchid3")
  }
  if (!is.null(edge_cols) & type == "signed") {
    if (length(edge_cols) != 2) {
      stop(paste0(length(edge_cols), " colors provided but 2 are needed"))
    }
  }
  if (!is.null(edge_cols) & type == "complex") {
    if (length(edge_cols) != 3) {
      stop(paste0(length(edge_cols), " colors provided but 3 are needed"))
    }
  }
  if (is.null(attr) & type == "complex") {
    stop('"attr" must be specified for type="complex"')
  }
  if (type == "signed") {
    if (weights) {
      igraph::E(g)$weight <- ifelse(igraph::E(g)$sign == 1, 3, 1)
    } else {
      igraph::E(g)$weight <- 1
    }
    ggraph::ggraph(g, "stress", weights = igraph::E(g)$weight) +
      ggraph::geom_edge_link0(ggplot2::aes_(col = ~ as.factor(sign))) +
      ggraph::geom_node_point(shape = 21, fill = "grey25", size = 5) +
      ggraph::scale_edge_color_manual(values = edge_cols) +
      ggraph::theme_graph() +
      ggplot2::theme(legend.position = "none")
  } else {
    if (weights) {
      igraph::E(g)$type <- igraph::get.edge.attribute(g, attr)
      igraph::E(g)$weight <- ifelse(igraph::E(g)$type == "P", 3, ifelse(igraph::E(g)$type == "A", 2, 1))
    } else {
      igraph::E(g)$weight <- 1
    }
    ggraph::ggraph(g, "stress", weights = igraph::E(g)$weight) +
      ggraph::geom_edge_link0(ggplot2::aes_(col = ~ as.factor(type))) +
      ggraph::geom_node_point(shape = 21, fill = "grey25", size = 5) +
      ggraph::scale_edge_color_manual(values = edge_cols) +
      ggraph::theme_graph() +
      ggplot2::theme(legend.position = "none")
  }
}
