# list signed triangles

lists all possible signed triangles

## Usage

``` r
signed_triangles(g)
```

## Arguments

- g:

  igraph object with a sign edge attribute.

## Value

matrix of vertex ids and the number of positive ties per triangle

## See also

[count_signed_triangles](https://schochastics.github.io/signnet/reference/count_signed_triangles.md)

## Author

David Schoch

## Examples

``` r
library(igraph)
g <- make_full_graph(4)
E(g)$sign <- c(-1, 1, 1, -1, -1, 1)
signed_triangles(g)
#>      V1 V2 V3 P
#> [1,]  1  2  4 1
#> [2,]  1  2  3 1
#> [3,]  1  3  4 3
#> [4,]  2  3  4 1
```
