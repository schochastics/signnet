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
#> IGRAPH 63239b6 U--- 30 75 -- 
#> + attr: grp (v/c), sign (e/n)
#> + edges from 63239b6:
#>  [1]  1-- 2  1-- 3  1-- 4  1-- 7  1-- 9  1--10  2-- 4  2-- 8  2-- 9  2--10
#> [11]  3-- 7  3-- 8  3--10  4-- 5  4-- 6  4-- 8  5-- 7  5-- 9  5--10  6-- 8
#> [21]  6-- 9  6--10  7-- 8  8--10  9--10 11--12 11--14 11--15 11--19 12--13
#> [31] 12--16 12--17 12--18 12--20 13--15 13--18 13--19 14--17 14--18 15--16
#> [41] 15--17 15--20 16--17 16--18 16--19 16--20 17--19 17--20 18--19 18--20
#> [51] 21--25 21--27 21--29 22--23 22--25 22--26 22--27 22--29 22--30 23--24
#> [61] 23--28 23--30 24--25 24--27 24--29 25--26 25--29 25--30 26--28 26--30
#> [71] 27--29 27--30  5--21  5--11  9--22
```
