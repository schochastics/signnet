# Bipartite random signed graphs

Bipartite random signed graphs

## Usage

``` r
sample_bipartite_signed(
  n1,
  n2,
  p,
  p_neg,
  directed = FALSE,
  mode = c("out", "in", "all")
)
```

## Arguments

- n1:

  Integer scalar, the number of bottom vertices.

- n2:

  Integer scalar, the number of top vertices.

- p:

  The probability for drawing an edge between two arbitrary vertices.

- p_neg:

  The probability of a drawn edge to be a negative tie

- directed:

  logical, whether the graph will be directed. defaults to FALSE.

- mode:

  Character scalar, specifies how to direct the edges in directed
  graphs. If it is ‘out’, then directed edges point from bottom vertices
  to top vertices. If it is ‘in’, edges point from top vertices to
  bottom vertices. ‘out’ and ‘in’ do not generate mutual edges. If this
  argument is ‘all’, then each edge direction is considered
  independently and mutual edges might be generated. This argument is
  ignored for undirected graphs.

## Value

A signed bipartite igraph graph.

## Examples

``` r
sample_bipartite_signed(10, 10, 0.5, 0.5)
#> IGRAPH f2519a0 U--B 20 52 -- Bipartite Gnp random graph
#> + attr: name (g/c), p (g/n), type (v/l), sign (e/n)
#> + edges from f2519a0:
#>  [1]  1--11  6--11  7--11  8--11  9--11  2--12  3--12  8--12 10--12  1--13
#> [11]  3--13  6--13  8--13 10--13  2--14  3--14  4--14  5--14  6--14  7--14
#> [21] 10--14  1--15  3--15  7--15  8--15  2--16  3--16  4--16  6--16  9--16
#> [31]  2--17  6--17  7--17 10--17  1--18  3--18  4--18  5--18  6--18  9--18
#> [41] 10--18  1--19  4--19  5--19  6--19  7--19  8--19  9--19 10--19  6--20
#> [51]  8--20 10--20
```
