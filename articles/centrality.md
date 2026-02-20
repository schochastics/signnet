# Centrality

This vignette describes the use of centrality in signed networks.

``` r
library(igraph)
library(signnet)
```

## Centrality indices for signed networks

There exist dozens of indices for networks with positive ties, but for
signed networks they are rather scarce. The package implements three
indices so far. Versions of degree and eigenvector centrality, and PN
centrality by Everett & Borgatti.

Degree centrality can be calculated in four different ways with
[`degree_signed()`](https://schochastics.github.io/signnet/reference/degree_signed.md),
specified by the `type` parameter:

- `type="pos"` count only positive neighbors
- `type="neg"` count only negative neighbors
- `type="ratio"` positive neighbors/(positive neighbors+negative
  neighbors)
- `type="net"` positive neighbors-negative neighbors

The `mode` parameter can be used to get “in” and “out” versions for
directed networks.

The PN index is very similar to Katz status and Hubbell’s measure for
networks with only positive ties. The technical details can be found in
the paper by Everett & Borgatti.

The below example illustrates all indices with a network where signed
degree can not distinguish vertices.

``` r
A <- matrix(c(0,  1,  0,  1,  0,  0,  0, -1, -1,  0,  
               1,  0,  1, -1,  1, -1, -1,  0,  0,  0,  
               0,  1,  0,  1, -1,  0,  0,  0, -1,  0,  
               1, -1,  1,  0,  1, -1, -1,  0,  0,  0,  
               0,  1, -1,  1,  0,  1,  0, -1,  0, -1,  
               0, -1,  0, -1,  1,  0,  1,  0,  1, -1,  
               0, -1,  0, -1,  0,  1,  0,  1, -1,  1,  
              -1,  0,  0,  0, -1,  0,  1,  0,  1,  0,  
              -1,  0, -1,  0,  0,  1, -1,  1,  0,  1,  
               0,  0,  0,  0, -1, -1,  1,  0,  1,  0),10,10)

g <- graph_from_adjacency_matrix(A,"undirected",weighted = "sign")

degree_signed(g,type="ratio")
#>  [1] 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5
eigen_centrality_signed(g)
#>  [1] -0.62214960  1.00000000 -0.74518850  1.00000000 -0.89990041  0.64289592
#>  [7]  0.35828159 -0.37471921 -0.28087411 -0.07834568
pn_index(g)
#>  [1] 0.9009747 0.8613482 0.9076997 0.8613482 0.8410658 0.8496558 0.8617321
#>  [8] 0.9015909 0.8509848 0.9072930
```

Note that PN centrality and eigenvector centrality differ significantly
for this network.

``` r
cor(eigen_centrality_signed(g),pn_index(g),method = "kendall")
#> [1] -0.1555556
```

## A note on eigenvector centrality

The adjacency matrix of a signed network may not have a dominant
eigenvalue. This means it is not clear which eigenvector should be used.
In addition it is possible for the adjacency matrix to have repeated
eigenvalues and hence multiple linearly independent eigenvectors. In
this case certain centralities can be arbitrarily assigned. The
[`eigen_centrality_signed()`](https://schochastics.github.io/signnet/reference/eigen_centrality_signed.md)
function returns an error if this is the case.

``` r
A <- matrix(c( 0,  1,  1, -1,  0,  0, -1,  0,  0, 
               1,  0,  1,  0, -1,  0,  0, -1,  0, 
               1,  1,  0,  0,  0, -1,  0,  0, -1, 
              -1,  0,  0,  0,  1,  1, -1,  0,  0, 
               0, -1,  0,  1,  0,  1,  0, -1,  0, 
               0,  0, -1,  1,  1,  0,  0,  0, -1, 
              -1,  0,  0, -1,  0,  0,  0,  1,  1, 
               0, -1,  0,  0, -1,  0,  1,  0,  1, 
               0,  0, -1,  0,  0, -1,  1,  1, 0), 9, 9)

g <- igraph::graph_from_adjacency_matrix(A,"undirected",weighted = "sign")
eigen_centrality_signed(g)
#> Error in `eigen_centrality_signed()`:
#> ! no dominant eigenvalue exists
```

## References

Everett, Martin G., and Stephen P. Borgatti. 2014. “Networks Containing
Negative Ties.” Social Networks 38: 111–20.

Bonacich, Phillip, and Paulette Lloyd. 2004. “Calculating Status with
Negative Relations.” Social Networks 26 (4): 331–38.
