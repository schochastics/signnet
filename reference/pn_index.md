# PN Centrality Index

centrality index for signed networks by Everett and Borgatti

## Usage

``` r
pn_index(g, mode = c("all", "in", "out"))
```

## Arguments

- g:

  igraph object with a sign edge attribute.

- mode:

  character string, “out” for out-pn, “in” for in-pn or “all” for
  undirected networks.

## Value

centrality scores as numeric vector.

## References

Everett, M. and Borgatti, S. (2014) Networks containing negative ties.
*Social Networks* 38 111-120

## Author

David Schoch

## Examples

``` r
A <- matrix(c(
    0, 1, 0, 1, 0, 0, 0, -1, -1, 0,
    1, 0, 1, -1, 1, -1, -1, 0, 0, 0,
    0, 1, 0, 1, -1, 0, 0, 0, -1, 0,
    1, -1, 1, 0, 1, -1, -1, 0, 0, 0,
    0, 1, -1, 1, 0, 1, 0, -1, 0, -1,
    0, -1, 0, -1, 1, 0, 1, 0, 1, -1,
    0, -1, 0, -1, 0, 1, 0, 1, -1, 1,
    -1, 0, 0, 0, -1, 0, 1, 0, 1, 0,
    -1, 0, -1, 0, 0, 1, -1, 1, 0, 1,
    0, 0, 0, 0, -1, -1, 1, 0, 1, 0
), 10, 10)
g <- graph_from_adjacency_matrix_signed(A,"undirected")
pn_index(g)
#>  [1] 0.9009747 0.8613482 0.9076997 0.8613482 0.8410658 0.8496558 0.8617321
#>  [8] 0.9015909 0.8509848 0.9072930
```
