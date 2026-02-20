# balancedness of signed network

Implements several indices to assess the balancedness of a network.

## Usage

``` r
balance_score(g, method = "triangles")
```

## Arguments

- g:

  igraph object with a sign edge attribute.

- method:

  string indicating the method to be used. See details for options

## Value

numeric balancedness score between 0 and 1

## Details

The method parameter can be one of

- *triangles*:

  Fraction of balanced triangles. Maximal (=1) if all triangles are
  balanced.

- *walk*:

  \\\sum exp(\lambda_i) / \sum exp(\mu_i)\\

- *frustration*:

  The frustration index assumes that the network can be partitioned into
  two groups, where intra group edges are positive and inter group edges
  are negative. The index is defined as the sum of intra group negative
  and inter group positive edges. Note that the problem is NP complete
  and only an upper bound is returned (based on simulated annealing).
  Exact methods can be found in the work of Aref. The index is
  normalized such that it is maximal (=1) if the network is balanced.

## References

Estrada, E. (2019). Rethinking structural balance in signed social
networks. *Discrete Applied Mathematics*.

Samin Aref, Mark C Wilson (2018). Measuring partial balance in signed
networks. *Journal of Complex Networks*, 6(4): 566–595,
https://doi.org/10.1093/comnet/cnx044

## Author

David Schoch

## Examples

``` r
library(igraph)
g <- make_full_graph(4)
E(g)$sign <- c(-1, 1, 1, -1, -1, 1)

balance_score(g, method = "triangles")
#> [1] 1
balance_score(g, method = "walk")
#> [1] 1
```
