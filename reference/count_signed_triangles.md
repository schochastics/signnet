# count signed triangles

Counts the number of all possible signed triangles (+++),(++-), (+–) and
(—)

## Usage

``` r
count_signed_triangles(g)
```

## Arguments

- g:

  igraph object with a sign edge attribute.

## Value

counts for all 4 signed triangle types

## See also

[signed_triangles](https://schochastics.github.io/signnet/reference/signed_triangles.md)

## Author

David Schoch

## Examples

``` r
library(igraph)
g <- make_full_graph(4)
E(g)$sign <- c(-1, 1, 1, -1, -1, 1)
count_signed_triangles(g)
#> +++ ++- +-- --- 
#>   1   0   3   0 
```
