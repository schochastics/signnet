---
title: 'signnet: An R package for analyzing signed networks'
tags:
- R
- network analysis
- signed networks
- structural balance theory
date: "02 November 2022"
output: pdf_document
bibliography: paper.bib
affiliations:
- name: GESIS - Leibniz Institute for the Social Sciences
  index: 1
authors:
- name: David Schoch
  orcid: 0000-0003-2952-4812
  affiliation: 1
---


# Summary

Network analysis usually deals with relations among entities which are positive,
including "friendship", or "advice seeking". Most analytic tools are constructed
with this assumption, be that centrality indices, or clustering tools. However,
not all conceivable relationships are positive. People can be friends but also
enemies. A signed network is a network where both, positive and negative
relationships may occur. Common network analytic tools are not applicable to
such networks without adapting for the existence of the negative ties. The R
package `signnet` package brings together methods that have been developed to
analyse signed networks. This includes known blockmodeling techniques, centrality
indices and tools for two-mode networks, as well as unique analytic techniques
surrounding structural balance theory. 

# Statement of need

Signed networks are increasingly popular in network science since many empirical
phenomena can be modeled with positive and negative ties. Examples include
studies of polarization [@n-stwspuc1-20], collaborations on Wikipedia
[@bklv-nacsw-09], and relations on social media [@klb-szmsnne-09]. General
purpose packages for network analysis such as `igraph` [@cn-ispcnr-06] and `sna`
 [@b-snas-08] implement all commonly used network analytic methods but do not
 offer any functionality for signed networks. The `signnet` package closes this
 gap and makes many tools for signed network available in R. 

# Implementation details
The package is modeled with compatibility to `igraph` in mind and follows its naming scheme. 
All functions in the package assume that an igraph object is a signed network if
it has an edge attribute "sign" with values 1 (positive) or -1 (negative). If a
function from igraph was adapted for signed networks, it can be called via
`<igraph_name>_signed()`. Prominent examples include `as_adj_signed()`,
`graph_from_adjacency_matrix_signed()`, `degree_signed()`, and
`triad_census_signed()`.

# Functionalities

This section explains the main methods implemented in the package. 
For more details for each subsection see the respective package vignette. 

```R
library(signnet)
data("tribes") # dataset included in signnet
```

## Structural balance
In its simplest form, structural balance is defined via triangles [@h-aco-46]. A
triangle in a network is balanced if all ties are positive ("the friend of a
friend is a friend") or only one tie is positive ("the enemy of my enemy is my
friend"). The remaining configurations are said to be unbalanced. A whole
network is balanced if it can be partitioned into two node sets, such that
intra-group edges are all positive and inter-group edges are all negative
[@ch-sbght-56]. 

Determining if a network is balanced or not is easy, but measuring the degree of "balancedness" (i.e. how close is a network to be balanced?) is not. `signnet` implements several methods to calculate balance scores[@aw-mpbsn-18]. All are defined such that a value of one indicates perfect balance and zero perfect imbalance. 

```R
balance_score(tribes, method = "triangles")
#> 0.867647
```
The method based on triangles simply counts the fraction of triangles that are balanced.
Alternatively, the frustration index can be used, which computes the minimum number of edges 
whose removal results in a balance network [@aw-bfsn-19]. 

```R
frustration_exact(tribes)
#> $frustration
#> [1] 7
#> 
#> $partition
#>  [1] 0 0 1 1 1 1 1 1 1 1 1 1 1 1 0 0
```

The return value `partition` gives the optimal partition into the two node sets for 
which the optimal frustration is achieved. Note that the problem is NP hard and 
frustration can thus only be calculated for rather small signed networks.

## Blockmodeling
In signed blockmodeling, the goal is to determine $k$ blocks of nodes such that
all intra-block edges are positive and inter-block edges are negative
[@dm-pasb-96]. The function `signed_blockmodel()` is used to construct such a
model. The parameter $k$ is the number of desired blocks. $\alpha$ is a trade-off
parameter. The function minimizes $P(C)=\alpha N+(1-\alpha)P$, where $N$ is the
total number of negative ties within blocks and $P$ be the total number of
positive ties between blocks. 

```R
tribe_blocks <- signed_blockmodel(tribes, k = 3, 
                                  alpha = 0.5, annealing = TRUE)
tribe_blocks
#> $membership
#> [1] 2 2 1 1 3 1 1 1 3 3 1 1 3 3 2 2
#> 
#> $criterion
#> [1] 2
```

The result can be visualized with `ggblock`, as shown in Figure \ref{fig:block_ex}.
```R
ggblock(tribes, tribe_blocks$membership, show_blocks = TRUE)
```
\begin{figure}[htb]
  \centering
  \includegraphics[width=0.5\textwidth]{man/figures/block_example.pdf}
  \caption{Blocks of the tribes network. Blue squares indicate positive and red
  squares negative ties.}
  \label{fig:block_ex}
\end{figure}

## Centrality
There exist hundreds of indices for networks with only positive ties, but for signed
networks they are rather scarce. The `signnet` package implements three indices.
Versions of degree and eigenvector centrality [@bl-csnr-04], and PN centrality [@eb-ncnt-14].
The PN index is very similar to Katz status for networks with only positive ties
[@k-nsidsa-53]. The technical details can be found in the paper by Everett &
Borgatti.

The below example illustrates all indices with a network where signed degree can not distinguish vertices.
```R
A <- matrix(c( 0,  1,  0,  1,  0,  0,  0, -1, -1,  0,  
               1,  0,  1, -1,  1, -1, -1,  0,  0,  0,  
               0,  1,  0,  1, -1,  0,  0,  0, -1,  0,  
               1, -1,  1,  0,  1, -1, -1,  0,  0,  0,  
               0,  1, -1,  1,  0,  1,  0, -1,  0, -1,  
               0, -1,  0, -1,  1,  0,  1,  0,  1, -1,  
               0, -1,  0, -1,  0,  1,  0,  1, -1,  1,  
              -1,  0,  0,  0, -1,  0,  1,  0,  1,  0,  
              -1,  0, -1,  0,  0,  1, -1,  1,  0,  1,  
               0,  0,  0,  0, -1, -1,  1,  0,  1,  0),10,10)

g <- graph_from_adjacency_matrix_signed(A, "undirected")

degree_signed(g, type = "ratio")
#> [1] 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5
eigen_centrality_signed(g)
#> [1] 0.6221496 1.0000000 0.7451885 1.0000000 0.8999004 0.6428959 0.3582816
#> [8] 0.3747192 0.2808741 0.0783457
pn_index(g)
#> [1] 0.900975 0.861348 0.907700 0.861348 0.841066 0.849656 0.861732 
#> [8] 0.901591 0.850985 0.907293
```

## Signed two-mode networks
A common analytic tool for two-mode networks is to project the network onto on
relevant mode. This is easily done using the adjacency matrix $𝐴$. $𝐴𝐴^𝑇$
yields the row projection and $𝐴^𝑇𝐴$ the column projection. The resulting
networks will thus be weighted. Several methods exist to turn a weighted
projection into an unweighted network where only the most significant edges are
included [@dns-brpebbp-21]. Projecting signed networks is not as
straightforward, because "nullification" of edges can occur. @s-pstn-21
introduces two methods to deal with this issue which are implemented in
`signnet`. The trick is to convert the signed network into an a special
unsigned one with `as_unsigned_2mode()`, do the projection as usual and the turn
it back to a signed graph with `as_signed_proj()`. The details can be found in
the original paper and in a designated vignette.

# References
