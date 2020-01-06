#' @title count signed triangles
#' @description Counts the number of all possible signed triangles (+++),(++-), (+--) and (---)
#'
#' @param g igraph object with signed edge attribute
#' @return counts for all 4 signed triangle types
#' @author David Schoch
#' @seealso [signed_triangles]
#' @examples
#' library(igraph)
#' g <- graph.full(4)
#' E(g)$sign <- c(-1,1,1,-1,-1,1)
#' count_signed_triangles(g)
#' @export
count_signed_triangles <- function(g){
  if(!"sign"%in%igraph::edge_attr_names(g)){
    stop("network does not have a sign edge attribute")
  }
  if(igraph::is.directed(g)){
    stop("g must be undirected")
  }
  eattrV <- igraph::get.edge.attribute(g,"sign")
  if(!all(eattrV%in%c(-1,1))){
    stop("sign may only contain -1 and 1")
  }

  tmat <- t(matrix(igraph::triangles(g),nrow=3))
  if(nrow(tmat)==0){
    warning("g does not contain any triangles")
    return(c("+++" = 0,"++-" = 0,"+--" = 0,"---" = 0))
  }
  emat <- t(apply(tmat,1,function(x) c(igraph::get.edge.ids(g,x[1:2]),
                                       igraph::get.edge.ids(g,x[2:3]),
                                       igraph::get.edge.ids(g,x[c(3,1)]))))


  emat[,1] <- eattrV[emat[,1]]
  emat[,2] <- eattrV[emat[,2]]
  emat[,3] <- eattrV[emat[,3]]
  emat <- t(apply(emat,1,sort))
  emat_df <- as.data.frame(emat)
  res <- stats::aggregate(list(count=rep(1,nrow(emat_df))), emat_df, length)

  tri_counts <- c("+++" = 0,"++-" = 0,"+--" = 0,"---" = 0)

  tmp_counts <- res[,4]
  if(nrow(res)==1){
    names(tmp_counts) <- paste0(c("+","-")[(rev(res[1:3])==-1)+1],collapse="")
  } else{
    names(tmp_counts) <- apply(res[,1:3],1,function(x) paste0(c("+","-")[(rev(x)==-1)+1],collapse=""))
  }

  tri_counts[match(names(tmp_counts),names(tri_counts))] <- tmp_counts
  tri_counts
}

#' @title list signed triangles
#' @description lists all possible signed triangles
#'
#' @param g igraph object with signed edge attribute
#' @return matrix of vertex ids and the number of positive ties per triangle
#' @author David Schoch
#' @seealso [count_signed_triangles]
#' @examples
#' library(igraph)
#' g <- graph.full(4)
#' E(g)$sign <- c(-1,1,1,-1,-1,1)
#' signed_triangles(g)
#' @export
signed_triangles <- function(g){
  if(!"sign"%in%igraph::edge_attr_names(g)){
    stop("network does not have a sign edge attribute")
  }
  if(igraph::is.directed(g)){
    stop("g must be undirected")
  }
  eattrV <- igraph::get.edge.attribute(g,"sign")
  if(!all(eattrV%in%c(-1,1))){
    stop("sign may only contain -1 and 1")
  }

  tmat <- t(matrix(igraph::triangles(g),nrow=3))
  if(nrow(tmat)==0){
    warning("g does not contain any triangles")
    return(NULL)
  }
  emat <- t(apply(tmat,1,function(x) c(igraph::get.edge.ids(g,x[1:2]),
                                       igraph::get.edge.ids(g,x[2:3]),
                                       igraph::get.edge.ids(g,x[c(3,1)]))))

  semat <- matrix(0,nrow(emat),3)
  semat[,1] <- eattrV[emat[,1]]
  semat[,2] <- eattrV[emat[,2]]
  semat[,3] <- eattrV[emat[,3]]
  cls <- apply(semat, 1, function(v) length(which(v==1)))
  emat <- cbind(emat,unname(cls))
  colnames(emat) <- c("V1","V2","V3","P")
  emat
}

#' @title count complex triangles
#' @description Counts the number of all possible signed triangles (+++),(++-), (+--) and (---)
#'
#' @param g igraph object.
#' @param attr edge attribute name that encodes positve ("P"), negative ("N") and ambivalent ("A") ties.
#' @return counts for all complex triangle types
#' @author David Schoch
#' @seealso [signed_triangles]
#' @examples
#' library(igraph)
#' g <- graph.full(4)
#' E(g)$type <- c("P","N","A","A","P","N")
#' count_complex_triangles(g,attr = "type")
#' @export
count_complex_triangles <- function(g,attr){
  if(missing(attr)){
    stop('argument "attr" is missing, with no default')
  }
  if(igraph::is.directed(g)){
    stop("g must be undirected")
  }
  eattrV <- igraph::get.edge.attribute(g,attr)
  if(!all(eattrV%in%c("P","N","A"))){
    stop('attr may only contain "P","N" and "A" ')
  }

  tmat <- t(matrix(igraph::triangles(g),nrow=3))
  if(nrow(tmat)==0){
    warning("g does not contain any triangles")
    return(c("PPP" = 0, "PPN" = 0, "PNN" = 0, "NNN" = 0,
             "PPA" = 0, "PNA" = 0, "NNA" = 0, "PAA" = 0, "NAA" = 0,
             "AAA" = 0))
  }
  emat <- t(apply(tmat,1,function(x) c(igraph::get.edge.ids(g,x[1:2]),
                                       igraph::get.edge.ids(g,x[2:3]),
                                       igraph::get.edge.ids(g,x[c(3,1)]))))


  semat <- matrix(0,nrow(emat),3)
  semat[,1] <- eattrV[emat[,1]]
  semat[,2] <- eattrV[emat[,2]]
  semat[,3] <- eattrV[emat[,3]]
  semat <- t(apply(semat,1,sort,decreasing=T))

  emat_df <- as.data.frame(semat)
  res <- stats::aggregate(list(count=rep(1,nrow(emat_df))), emat_df, length)
  tmp_counts <- res[,4]

  if(nrow(res)==1){
    names(tmp_counts) <- paste0(res[1:3],collapse="")
  } else{
    names(tmp_counts) <- apply(res[,1:3],1,function(x) paste0(x,collapse=""))
  }

  tri_counts <- c("PPP" = 0, "PPN" = 0, "PNN" = 0, "NNN" = 0,
                  "PPA" = 0, "PNA" = 0, "NNA" = 0, "PAA" = 0, "NAA" = 0,
                  "AAA" = 0)

  tri_counts[match(names(tmp_counts),names(tri_counts))] <- tmp_counts
  tri_counts
}
