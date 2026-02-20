# count complex triangles

Counts the number of all possible signed triangles (+++),(++-), (+–) and
(—)

## Usage

``` r
count_complex_triangles(g, attr)
```

## Arguments

- g:

  igraph object.

- attr:

  edge attribute name that encodes positive ("P"), negative ("N") and
  ambivalent ("A") ties.

## Value

counts for all complex triangle types

## See also

[signed_triangles](https://schochastics.github.io/signnet/reference/signed_triangles.md)

## Author

David Schoch

## Examples

``` r
library(igraph)
g <- make_full_graph(4)
E(g)$type <- c("P", "N", "A", "A", "P", "N")
count_complex_triangles(g, attr = "type")
#> PPP PPN PNN NNN PPA PNA NNA PAA NAA AAA 
#>   0   0   0   0   1   2   1   0   0   0 
```
