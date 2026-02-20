# Signed Networks

This vignette describes signed networks and how they are implemented in
the `signnet` package.

``` r
library(igraph)
library(signnet)
```

## What are signed networks?

Traditional SNA usually deals with relations among entities
(e.g. people) that are positive, including “friendship”, “advice
seeking”, etc. Most network analytic tools are devised under this
premise, be that centrality indices, clustering tools and so forth. But
of course not all occurring relations are positive. People can be
friends but also foes.

This gives rise to signed networks. These networks are usually composed
of both, positive and negative, ties measured among a set of entities.
Traditional network analytic tools are not applicable to such networks
without adapting for negative ties.

The `signnet` package brings together methods that have been developed
to analyse signed networks. This includes

- Structural balance
  ([vignette](https://schochastics.github.io/signnet/articles/structural_balance.md))
- Blockmodeling
  ([vignette](https://schochastics.github.io/signnet/articles/blockmodeling.md))
- Centrality
  ([vignette](https://schochastics.github.io/signnet/articles/centrality.md))
- Signed two-mode networks
  ([vignette](https://schochastics.github.io/signnet/articles/signed_2mode.md))
- Complex matrices
  ([vignette](https://schochastics.github.io/signnet/articles/complex_matrices.md))

## Data structures for signed networks in `signnet`

The foundation of `signnet` is provided by `igraph`. All functions in
the package assume that an igraph object is a signed network if it has
an edge attribute “sign” with values 1 (positive) or -1 (negative).

``` r
g <- make_full_graph(5,directed = FALSE,loops = FALSE)
E(g)$sign <- 1
g
#> IGRAPH 922b11d U--- 5 10 -- Full graph
#> + attr: name (g/c), loops (g/l), sign (e/n)
#> + edges from 922b11d:
#>  [1] 1--2 1--3 1--4 1--5 2--3 2--4 2--5 3--4 3--5 4--5
```

All methods (should) throw an error if the sign attribute is missing or
contains other values than -1 and 1.

Matrices associated with a signed network follow the `igraph` naming
scheme. The signed adjacency matrix can be obtained with
[`as_adj_signed()`](https://schochastics.github.io/signnet/reference/as_adj_signed.md).

``` r
data("tribes")
as_adj_signed(tribes)[1:5,1:5]
#>       Gavev Kotun Ove Alika Nagam
#> Gavev     0     1  -1    -1    -1
#> Kotun     1     0  -1     0    -1
#> Ove      -1    -1   0     1     0
#> Alika    -1     0   1     0     0
#> Nagam    -1    -1   0     0     0
```

The signed Laplacian matrix is obtained by
[`laplacian_matrix_signed()`](https://schochastics.github.io/signnet/reference/laplacian_matrix_signed.md).

``` r
laplacian_matrix_signed(tribes)[1:5,1:5]
#>       Gavev Kotun Ove Alika Nagam
#> Gavev     8    -1   1     1     1
#> Kotun    -1     8   1     0     1
#> Ove       1     1   6    -1     0
#> Alika     1     0  -1     3     0
#> Nagam     1     1   0     0     7
```

## Included datasets

The package includes two well known datasets.

The “tribes” dataset is a signed social network of tribes of the
Gahuku–Gama alliance structure of the Eastern Central Highlands of New
Guinea. The network contains sixteen tribes connected by friendship
(“rova”) and enmity (“hina”).

The “cowList” dataset contains a list of 52 signed networks of
inter-state relations over time (1946-1999). Two countries are connected
by a positive tie if they form an alliance or have a peace treaty. A
negative tie exists between countries who are at war or in other kinds
of conflicts. The dataset is derrived from the [correlates of
war](https://correlatesofwar.org/).
