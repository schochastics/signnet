#' Signed networks from Correlates of War
#' @description 51 signed networks of inter state relations

#' @format List of igraph objects
#' @source http://mrvar.fdv.uni-lj.si/pajek/SVG/CoW/default.htm
#' @references Doreian, P. and Mrvar, A. (2015). "Structural Balance and Signed International Relations". *Journal of Social Structure*, 16(2)
"cowList"



# code to transform data into igraph object
# library(tidyverse)
# library(igraph)
# txt <- readLines("Cow.net")
#
# vert <- tibble(raw=txt[2:181]) %>%
#   mutate(raw=str_trim(raw)) %>%
#   separate(raw,c("id","country","x","y","z"),sep="\\s+") %>%
#   mutate(country=str_remove_all(country,'\\"')) %>%
#   select(-c(x,y,z))
#
# eids <- c(str_which(txt,"Edges"),length(txt)+1)
#
# map(1:(length(eids)-1),function(x){
#   txt[(eids[x]+1):(eids[x+1]-1)] %>%
#     tibble(raw=.) %>%
#     mutate(raw=str_trim(raw)) %>%
#     separate(raw,c("from","to","sign"),sep="\\s+")
#
# }) ->edges
#
# years <- txt[eids] %>%
#   .[!is.na(.)] %>%
#   str_remove_all("\\*Edges :[0-9]+ ") %>%
#   str_remove_all('\\"')
#
# map(edges,function(x){
#   g <- graph_from_data_frame(x,FALSE,vert)
# }) -> cowList
#
# map(cowList,function(x){
#   V(x)$name <- V(x)$country
#   x <- delete_vertex_attr(x,"country")
#   x
# })->cowList
#
# for(i in 1:length(cowList)){
#   cowList[[i]]$year <- years[i]
# }
# names(cowList) <- years
# map(cowList,function(x){
#   igraphUtils::delete_isolates(x)
# }) -> cowList
# map(cowList,function(x){
#   E(x)$sign1 <- as.numeric(E(x)$sign)
#   x <- delete_edge_attr(x,"sign")
#   E(x)$sign <- E(x)$sign1
#   x <- delete_edge_attr(x,"sign1")
#   x
# }) ->cowList
