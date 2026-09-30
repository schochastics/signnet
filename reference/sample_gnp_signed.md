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
#> IGRAPH 3add134 U--- 10 19 -- Erdos-Renyi (gnp) graph
#> + attr: name (g/c), type (g/c), loops (g/l), p (g/n), sign (e/n)
#> + edges from 3add134:
#>  [1] 1-- 2 1-- 3 2-- 3 3-- 4 2-- 5 4-- 6 2-- 7 3-- 7 2-- 8 3-- 8 5-- 8 6-- 8
#> [13] 1-- 9 2-- 9 2--10 3--10 4--10 6--10 8--10
```
