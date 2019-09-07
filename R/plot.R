#' @title Plot Blockmodel matrix
#' @param g igraph object. Must have a "sign" edge attribute.
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
#' clu <- signed_blockmodel(tribes,k = 3,alpha=0.5,annealing = TRUE)
#' ggblock(tribes,clu$membership,show_blocks = TRUE,show_labels = TRUE)
#' }
#' @export
#'
ggblock <- function(g,blocks = NULL,cols = NULL,show_blocks = FALSE,show_labels = FALSE){
  if(!requireNamespace("ggplot2", quietly = TRUE)){
    stop("The package 'ggplot2' is needed for this function.")
  }
  if(is.null(cols)){
    cols <- c("firebrick","steelblue")
  }
  if(is.null(blocks)){
    permI <- 1:igraph::vcount(g)
    blocks <- rep(1,igraph::vcount(g))
  } else{
    permI <- order(blocks)
  }
  if(show_blocks){
    bsizes <- unname(table(blocks))
    bsizes <- cumsum(bsizes)+0.5
    rsizes <- igraph::vcount(g)-bsizes[-length(bsizes)]
    csizes <- bsizes[-length(bsizes)]
  }
  if(!"name"%in%igraph::vertex_attr_names(g)){
    g <- igraph::set_vertex_attr(g,"name",value=1:igraph::vcount(g))
  }
  A <- as.matrix(igraph::as_adj(g,type = "both",attr="sign"))
  df <- data.frame(from=rep(rownames(A),ncol(A)),to=rep(colnames(A),each=nrow(A)),value=c(A))
  df[["from"]] <- factor(df[["from"]],levels=rev(rownames(A)[permI]))
  df[["to"]] <- factor(df[["to"]],levels=colnames(A)[permI])
  df <- df[df[["value"]]!=0,]
  df[["value"]] <- as.factor(df[["value"]])
  p <- ggplot2::ggplot(df,ggplot2::aes_(y=~from,x=~to))+
    ggplot2::geom_tile(ggplot2::aes_(fill=~value),col="white")+
    ggplot2::scale_fill_manual(values=cols)+
    ggplot2::theme_void()+
    ggplot2::theme(legend.position = "none")+
    ggplot2::coord_fixed()
  if(show_blocks){
    p <- p + ggplot2::geom_vline(xintercept = csizes)+ggplot2::geom_hline(yintercept = rsizes)
  }
  if(show_labels){
    p <- p + ggplot2::theme(axis.text = ggplot2::element_text())
  }
  p
}
