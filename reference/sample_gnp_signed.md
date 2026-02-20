# Generate random signed graphs according to the G(n,p) Erdos-Renyi model

Generate random signed graphs according to the G(n,p) Erdos-Renyi model

## Usage

``` r
sample_gnp_signed(n, p, p_neg, directed = FALSE, loops = FALSE)
```

## Arguments

- n:

  The number of vertices in the graph.

- p:

  The probability for drawing an edge between two arbitrary vertices.

- p_neg:

  The probability of a drawn edge to be a negative tie

- directed:

  logical, whether the graph will be directed. defaults to FALSE.

- loops:

  logical, whether to add loop edges, defaults to FALSE.

## Value

a signed igraph graph object

## References

Erdos, P. and Renyi, A., On random graphs, *Publicationes Mathematicae
6*, 290–297 (1959).

## Examples

``` r
sample_gnp_signed(10, 0.4, 0.5)
#> IGRAPH a001352 U--- 10 16 -- Erdos-Renyi (gnp) graph
#> + attr: name (g/c), type (g/c), loops (g/l), p (g/n), sign (e/n)
#> + edges from a001352:
#>  [1] 1-- 3 2-- 3 2-- 4 1-- 5 1-- 6 2-- 6 4-- 6 4-- 7 6-- 7 2-- 8 5-- 8 1-- 9
#> [13] 4-- 9 6--10 7--10 8--10
```
