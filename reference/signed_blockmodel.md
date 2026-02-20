# Blockmodeling for signed networks

Finds blocks of nodes with intra-positive and inter-negative edges

## Usage

``` r
signed_blockmodel(g, k, alpha = 0.5, annealing = FALSE)
```

## Arguments

- g:

  igraph object with a sign edge attribute.

- k:

  number of blocks

- alpha:

  see details

- annealing:

  logical. if TRUE, use simulated annealing (Default: FALSE)

## Value

numeric vector of block assignments and the associated criterion value

## Details

The function minimizes P(C)=\\\alpha\\N+(1-\\\alpha\\)P, where N is the
total number of negative ties within plus-sets and P be the total number
of positive ties between plus-sets. This function implements the
structural balance model. That is, all diagonal blocks are positive and
off-diagonal blocks negative. For the generalized version see
[signed_blockmodel_general](https://schochastics.github.io/signnet/reference/signed_blockmodel_general.md).

## References

Doreian, Patrick and Andrej Mrvar (2009). Partitioning signed social
networks. *Social Networks* 31(1) 1-11

## Author

David Schoch

## Examples

``` r
library(igraph)

g <- sample_islands_signed(10, 10, 1, 20)
clu <- signed_blockmodel(g, k = 10, alpha = 0.5)
table(clu$membership)
#> 
#>  1  2  3  4  5  6  7  8  9 10 
#> 10 10 10 10 10 10 10 10 10 10 
clu$criterion
#> [1] 0

# Using simulated annealing (less change of getting trapped in local optima)
data("tribes")
clu <- signed_blockmodel(tribes, k = 3, alpha = 0.5, annealing = TRUE)
table(clu$membership)
#> 
#> 1 2 3 
#> 5 7 4 
clu$criterion
#> [1] 2
```
