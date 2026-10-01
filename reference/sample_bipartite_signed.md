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
#> IGRAPH 869d09c U--B 20 54 -- Bipartite Gnp random graph
#> + attr: name (g/c), p (g/n), type (v/l), sign (e/n)
#> + edges from 869d09c:
#>  [1]  1--11  2--11  6--11  8--11 10--11  2--12  3--12  4--12  7--12  8--12
#> [11]  9--12 10--12  1--13  3--13  5--13  9--13 10--13  1--14  3--14  4--14
#> [21]  6--14  7--14  8--14  1--15  2--15  4--15  6--15  7--15  9--15  1--16
#> [31]  2--16  3--16  5--16  6--16  9--16 10--16  1--17  6--17  9--17  2--18
#> [41]  4--18  7--18  9--18 10--18  2--19  4--19  7--19  8--19 10--19  2--20
#> [51]  3--20  5--20  6--20  9--20
```
