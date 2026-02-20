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
#> IGRAPH 0f07f73 U--B 20 51 -- Bipartite Gnp random graph
#> + attr: name (g/c), p (g/n), type (v/l), sign (e/n)
#> + edges from 0f07f73:
#>  [1]  1--11  2--11  3--11  4--11  5--11  8--11  9--11  1--12  5--12  6--12
#> [11] 10--12  1--13  2--13  4--13  7--13 10--13  4--14  5--14  8--14  9--14
#> [21]  1--15  2--15  3--15  4--15  7--15  8--15  9--15  2--16  3--16  4--16
#> [31]  5--16  6--16  7--16  8--16  4--17  6--17  8--17 10--17  4--18  5--18
#> [41]  6--18  7--18  8--18  4--19  5--19  7--19  8--19  9--19  4--20  6--20
#> [51] 10--20
```
