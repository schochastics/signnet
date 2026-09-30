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
#> IGRAPH ae6c26c U--- 10 15 -- Erdos-Renyi (gnp) graph
#> + attr: name (g/c), type (g/c), loops (g/l), p (g/n), sign (e/n)
#> + edges from ae6c26c:
#>  [1] 1-- 2 1-- 3 3-- 4 3-- 5 2-- 6 4-- 6 5-- 6 2-- 7 5-- 7 1-- 8 3-- 8 1-- 9
#> [13] 5-- 9 6-- 9 6--10
```
