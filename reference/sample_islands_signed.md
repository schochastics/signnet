# A graph with random subgraphs connected by negative edges

Create a number of Erdos-Renyi random graphs with identical parameters,
and connect them with the specified number of negative ties.

## Usage

``` r
sample_islands_signed(islands.n, islands.size, islands.pin, n.inter)
```

## Arguments

- islands.n:

  The number of islands in the graph.

- islands.size:

  The size of the islands in the graph.

- islands.pin:

  The probability of intra-island edges.

- n.inter:

  number of negative edges between two islands.

## Value

a signed igraph graph

## Author

David Schoch

## Examples

``` r
library(igraph)
sample_islands_signed(3, 10, 0.5, 1)
#> IGRAPH d626a4c U--- 30 76 -- 
#> + attr: grp (v/c), sign (e/n)
#> + edges from d626a4c:
#>  [1]  1-- 3  1-- 5  1-- 6  1-- 8  1-- 9  1--10  2-- 3  2-- 7  2-- 8  2-- 9
#> [11]  3-- 4  3-- 5  3-- 9  4-- 5  4-- 8  4-- 9  4--10  5-- 7  5-- 9  6-- 8
#> [21]  6-- 9  7--10  8-- 9 11--12 11--13 11--16 11--17 11--18 11--19 11--20
#> [31] 12--14 12--15 12--16 12--17 13--15 13--17 13--19 14--15 14--17 14--18
#> [41] 14--19 15--16 15--17 15--18 16--18 16--20 17--18 17--20 18--20 21--22
#> [51] 21--25 21--26 21--28 21--30 22--24 22--25 22--29 22--30 23--24 23--26
#> [61] 23--28 23--30 24--26 24--27 24--30 25--26 25--29 25--30 26--30 27--29
#> [71] 27--30 28--29 28--30  1--14  6--16 18--26
```
