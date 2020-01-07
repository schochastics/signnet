#' Convert a signed graph to a signed adjacency matrix
#'
#' This function returns the adjacency matrix for a signed graph
#'
#' @param g igraph object. Must have a "sign" edge attribute.
#' @param sparse 	Logical scalar, whether to return the result as a sparse matrix. The Matrix package is required for sparse matrices.
#' @return signed adjacency matrix
#' @seealso [as_adj_complex]
#' @export

as_adj_signed <- function(g,sparse = FALSE){
  if (!igraph::is_igraph(g)) {
    stop("Not a graph object")
  }
  if(!"sign"%in%igraph::edge_attr_names(g)){
    stop("network does not have a sign edge attribute")
  }
  igraph::as_adj(g,type = "both",attr = "sign", sparse = sparse)
}

#' Convert a signed two-mode network to a signed matrix
#'
#' This function returns the incidence matrix for a signed two-mode network.
#'
#' @param g igraph object (bipartite). Must have a "sign" edge attribute.
#' @param sparse 	Logical scalar, whether to return the result as a sparse matrix. The Matrix package is required for sparse matrices.
#' @return signed incidence matrix
#' @export

as_incidence_signed <- function(g,sparse = FALSE){
  if (!igraph::is_igraph(g)) {
    stop("Not a graph object")
  }
  if(!"sign"%in%igraph::edge_attr_names(g)){
    stop("network does not have a sign edge attribute")
  }
  if(!"type"%in%igraph::vertex_attr_names(g)){
    stop("network must have a type vertex attribute")
  }
  igraph::as_incidence_matrix(g,attr = "sign", sparse = sparse)
}


#' Convert a signed graph to a complex adjacency matrix
#'
#' This function returns the adjacency matrix for a signed graph that contains ambivalent ties
#'
#' @param g igraph object
#' @param attr edge attribute name that encodes positve ("P"), negative ("N") and ambivalent ("A") ties.
#' @return complex adjacency matrix
#' @seealso [as_adj_signed]
#' @export

as_adj_complex <- function(g,attr){
  if (!igraph::is_igraph(g)) {
    stop("Not a graph object")
  }
  if(igraph::is.directed(g)){
    stop("directed graphs are not supported")
  }
  if(missing(attr)){
    stop('argument "attr" is missing, with no default')
  }

  eattr <- igraph::get.edge.attribute(g,attr)
  if(!all(eattr%in%c("P","N","A"))){
    stop('attr may only contain "P","N" and "A" ')
  }
  A <- igraph::as_adj(g,type = "both",attr,sparse=FALSE)
  A <- replace(A,A=="P",complex(1,1,0))
  A <- replace(A,A=="N",complex(1,0,1))
  A <- replace(A,A=="A",complex(1,0.5,0.5))
  A <- replace(A,A=="",complex(1,0,0))
  class(A) <- "complex"
  A[lower.tri(A)] <- Conj(A[lower.tri(A)])
  A
}


#' @title Complex Graph Laplacian
#' @description The Laplacian of a signed graph containing ambivalent ties.
#' @param g igraph object.
#' @param attr edge attribute name that encodes positve ("P"), negative ("N") and ambivalent ("A") ties.
#' @param norm Whether to calculate the normalized Laplacian. See definitions below.
#' @details
#' See \link[igraph]{laplacian_matrix} of igraph for more details. In the complex case, D is a diagonal matrix containing the absolute values of row sums of the complex adjacency matrix.
#' @return a complex matrix
#' @seealso [laplacian_matrix_signed]
#' @author David Schoch
#' @export

laplacian_matrix_complex <- function(g,attr,norm = FALSE){
  if (!igraph::is_igraph(g)) {
    stop("Not a graph object")
  }
  if(igraph::is.directed(g)){
    stop("directed graphs are not supported")
  }
  A <- as_adj_complex(g,attr)
  I <- diag(1,nrow(A))
  D <- diag(rowSums(abs(A)))
  if(norm){
    diag(D) <- diag(D)^(-1/2)
    L <- I-D%*%A%*%D
  } else{
    L <- D-A
  }
  return(L)
}

#' @title Complex Incidence Matrix
#' @description The complex incidence matrix of a signed graph containing ambivalent ties.
#' @param g igraph object.
#' @param attr edge attribute name that encodes positve ("P"), negative ("N") and ambivalent ("A") ties.
#' @details
#' This function is slightly different than \link[igraph]{as_incidence_matrix} since it is defined for bipartite graphs.
#' The incidence matrix here is defined as a \eqn{S \in C^{n,m}}, where n is the number of vertices and m the number of edges. Edges (i,j) are oriented such that i<j and entries are defined as
#' \deqn{S_{i(i,j)}=\sqrt{A_{ij}}}
#' \deqn{S_{j(i,j)}=-\sqrt{A_{ji}} if (i,j) is an ambivalent tie}
#' \deqn{S_{j(i,j)}=-A_{ji}\sqrt{A_{ji}} else}
#' @return a complex matrix
#' @seealso [laplacian_matrix_complex],[as_adj_complex]
#' @author David Schoch
#' @export
as_incidence_complex <- function(g,attr){
  A <- as_adj_complex(g,attr)
  N <- igraph::vcount(g)
  M <- igraph::ecount(g)
  S <- matrix(0,N,M)
  e <- 0
  for(i in 1:N){
    for(j in i:N){
      if(A[i,j]!=0){
        e <- e+1
        if(A[i,j]==complex(1,1,0)){
          S[i,e] <- sqrt(A[i,j])
          S[j,e] <- -A[j,i]*sqrt(A[i,j])
        } else if(A[i,j]==complex(1,0,1)){
          S[i,e] <- sqrt(A[i,j])
          S[j,e] <- -A[j,i]*sqrt(A[i,j])
        }
        else {
          S[i,e] <- sqrt(A[i,j])
          S[j,e] <- -sqrt(A[j,i])
        }
      }
    }
  }
  S
}

#' @title Convert Signed Network to Complex
#' @param g igraph object. Must have a "sign" edge attribute.
#' @param attr new edge attribute name that encodes positve ("P"), negative ("N") and ambivalent ("A") ties.
#' @return igraph object
#' @author David Schoch
#' @examples
#' g <- sample_islands_signed(2,10,1,10)
#' as_complex_edges(g)
#' @export
as_complex_edges <- function(g,attr = "type"){
  if (!igraph::is_igraph(g)) {
    stop("Not a graph object")
  }
  if(!"sign"%in%igraph::edge_attr_names(g)){
    stop("network does not have a sign edge attribute")
  }
  esign <- igraph::get.edge.attribute(g,"sign")

  if(!all(esign%in%c(-1,1))){
    stop("sign may only contain -1 and 1")
  }

  ecompl <- ifelse(esign==1,"P","N")
  g <- igraph::set_edge_attr(g,name = attr,value = ecompl)
  g
}

#' @title Count Walks in complex signed network
#' @param g igraph object.
#' @param attr edge attribute that encodes positve ("P"), negative ("N") and ambivalent ("A") ties.
#' @param k integer. length of walks
#' @return igraph object
#' @author David Schoch
#' @examples
#' g <- sample_islands_signed(2,10,1,10)
#' g <- as_complex_edges(g,attr="type")
#' complex_walks(g,attr="type",k = 3)
#' @export
complex_walks <- function(g,attr,k){
  # if (!igraph::is_igraph(g)) {
  #   stop("Not a graph object")
  # }
  # if(missing(attr)){
  #   stop('argument "attr" is missing, with no default')
  # }
  # if(!attr%in%igraph::edge_attr_names(g)){
  #   stop(paste0('There is no edge attribute ','"',attr,'"'))
  # }
  # eattr <- igraph::get.edge.attribute(g,attr)
  # if(!all(eattr%in%c("P","N","A"))){
  #   stop('attr may only contain "P","N" and "A" ')
  # }
  A <- as_adj_complex(g,attr)
  B <- A
  for(i in 2:k){
    B <- cxmatmul(B,A)
  }
  return(B)
}

#' @title convert signed two-mode network to unsigned
#' @param g igraph object. Two-mode network, must have a "sign" edge attribute.
#' @param primary logical. Which mode to transform
#' @return igraph object
#' @author David Schoch
#' @seealso [as_signed_proj]
#' @examples
#' library(igraph)
#'
#' # create a simple signed two mode network
#' el <- matrix(c(1,"a",1,"b",1,"c",2,"a",2,"b"),ncol = 2,byrow = TRUE)
#' g <- graph_from_edgelist(el,directed = FALSE)
#' E(g)$sign <- c(1,1,-1,1,-1)
#' V(g)$type <- c(FALSE,TRUE,TRUE,TRUE,FALSE)
#'
#' # convert to unsigned two-mode network and project
#' l <- as_unsigned_2mode(g,primary = TRUE)
#' p <- bipartite_projection(l,which="true")
#'
#' # turn the unsigned projection back to a signed network
#' as_signed_proj(p)
#' @export

as_unsigned_2mode <- function(g,primary = TRUE){
  if (!igraph::is_igraph(g)) {
    stop("Not a graph object")
  }
  if(!"sign"%in%igraph::edge_attr_names(g)){
    stop("network does not have a sign edge attribute")
  }

  if(!"type"%in%igraph::vertex_attr_names(g)){
    stop("not a two-mode network.")
  }
  if(!"name"%in%igraph::vertex_attr_names(g)){
    igraph::V(g)$name <- as.character(1:igraph::vcount(g))
  }
  vnames <- igraph::V(g)$name
  mode1 <- vnames[which(igraph::V(g)$type)]
  mode2 <- vnames[which(!igraph::V(g)$type)]
  el <- igraph::as_data_frame(g,"edges")
  el[["sign"]] <- ifelse(el[["sign"]]==1,"-pos","-neg")
  from <- el[["from"]]
  to <- el[["to"]]
  signs <- el[["sign"]]

  if(primary){
    new_nodes <- paste0(rep(mode1,each=2),rep(c("-pos","-neg"),length(mode1)))
    vert <- data.frame(name=c(new_nodes,mode2),
                       type=c(rep(TRUE,length(new_nodes)),rep(FALSE,length(mode2))))
    from[from%in%mode1] <- paste0(from[from%in%mode1],signs[from%in%mode1])
    to[to%in%mode1] <- paste0(to[to%in%mode1],signs[to%in%mode1])
  } else{
    new_nodes <- paste0(rep(mode2,each=2),rep(c("-pos","-neg"),length(mode1)))
    vert <- data.frame(name=c(new_nodes,mode1),
                       type=c(rep(TRUE,length(new_nodes)),rep(FALSE,length(mode1))))
    from[from%in%mode2] <- paste0(from[from%in%mode2],signs[from%in%mode2])
    to[to%in%mode2] <- paste0(to[to%in%mode2],signs[to%in%mode2])
  }

  el <- data.frame(from,to)

  igraph::graph_from_data_frame(el,igraph::is.directed(g),vert)
}

#' @title convert unsigned projection to signed
#' @param g igraph object
#' @return igraph object
#' @author David Schoch
#' @seealso [as_unsigned_2mode]
#' @examples
#' library(igraph)
#'
#' # create a simple signed two mode network
#' el <- matrix(c(1,"a",1,"b",1,"c",2,"a",2,"b"),ncol = 2,byrow = TRUE)
#' g <- graph_from_edgelist(el,directed = FALSE)
#' E(g)$sign <- c(1,1,-1,1,-1)
#' V(g)$type <- c(FALSE,TRUE,TRUE,TRUE,FALSE)
#'
#' # convert to unsigned two-mode network and project
#' l <- as_unsigned_2mode(g,primary = TRUE)
#' p <- bipartite_projection(l,which="true")
#'
#' # turn the unsigned projection back to a signed network
#' as_signed_proj(p)
#' @export
as_signed_proj <- function(g){
  el <- igraph::as_data_frame(g,"edges")
  m1 <- regexpr("pos|neg",el[["from"]])
  m2 <- regexpr("pos|neg",el[["to"]])
  el[["type1"]] <- regmatches(el[["from"]],m1)
  el[["type2"]] <- regmatches(el[["to"]],m2)
  el[["type"]] <- "N"
  el[["type"]][el[["type1"]]==el[["type2"]]] <- "P"
  el[["from"]] <- gsub("\\-.*","",el[["from"]])
  el[["to"]] <- gsub("\\-.*","",el[["to"]])

  el <- el[,c("from","to","type")]

  el_new <- as.data.frame(t(apply(el[,1:2],1,sort)),stringsAsFactors = FALSE)
  el_new[["type"]] <- el[["type"]]
  el_new <- el_new[!duplicated(el_new),]
  names(el_new)[1:2] <- c("from","to")

  el_aggr <- stats::aggregate(cbind(count = type) ~ from+to, data = el_new,
                       FUN = function(x){c(NROW(x),c("N","P")[x[1]])})
  el_aggr <- do.call(data.frame,el_aggr)
  names(el_aggr)[c(3,4)] <- c("count","type")
  el_aggr[["from"]] <- as.character(el_aggr[["from"]])
  el_aggr[["to"]]   <- as.character(el_aggr[["to"]])
  el_aggr[["type"]] <- as.character(el_aggr[["type"]])
  el_aggr[["count"]] <- as.numeric(as.character(el_aggr[["count"]]))
  el_aggr[["type"]][el_aggr[["count"]]>1] <- "A"
  igraph::graph_from_data_frame(el_aggr[,c("from","to","type")],directed=FALSE)
}
