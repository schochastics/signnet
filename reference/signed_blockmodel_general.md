# Generalized blockmodeling for signed networks

Finds blocks of nodes with specified inter/intra group ties

## Usage

``` r
signed_blockmodel_general(g, blockmat, alpha = 0.5)
```

## Arguments

- g:

  igraph object with a sign edge attribute.

- blockmat:

  Integer Matrix. Specifies the inter/intra group patterns of ties. Must
  be square, contain only -1 and 1 and be symmetric for undirected
  networks.

- alpha:

  see details

## Value

list with the block assignments (`membership`) and the associated
criterion value (`criterion`)

## Details

The function minimizes P(C)=\\\alpha\\N+(1-\\\alpha\\)P, where N is the
total number of negative ties within positive blocks and P be the total
number of positive ties within negative blocks. This function implements
the generalized model. For the structural balance version see
[signed_blockmodel](https://schochastics.github.io/signnet/reference/signed_blockmodel.md).
Ties are counted per entry of the adjacency matrix, so each undirected
tie counts twice. The optimization uses simulated annealing followed by
a greedy local search and starts from a random partition. Use
[`set.seed()`](https://rdrr.io/r/base/Random.html) for reproducible
results.

## References

Doreian, Patrick and Andrej Mrvar (2009). Partitioning signed social
networks. *Social Networks* 31(1) 1-11

## Author

David Schoch

## Examples

``` r
library(igraph)
# create a signed network with three groups and different inter/intra group ties
g1 <- g2 <- g3 <- make_full_graph(5)

V(g1)$name <- as.character(1:5)
V(g2)$name <- as.character(6:10)
V(g3)$name <- as.character(11:15)

g <- Reduce("%u%", list(g1, g2, g3))
E(g)$sign <- 1
E(g)$sign[1:10] <- -1
g <- add_edges(g, c(rbind(1:5, 6:10)), attr = list(sign = -1))
g <- add_edges(g, c(rbind(1:5, 11:15)), attr = list(sign = -1))
g <- add_edges(g, c(rbind(11:15, 6:10)), attr = list(sign = 1))

# specify the link patterns between groups
blockmat <- matrix(c(1, -1, -1, -1, 1, 1, -1, 1, -1), 3, 3, byrow = TRUE)
signed_blockmodel_general(g, blockmat, 0.5)
#> $membership
#>  [1] 1 1 1 1 1 2 2 2 2 2 3 3 3 3 3
#> 
#> $criterion
#> [1] 0
#> 
```
