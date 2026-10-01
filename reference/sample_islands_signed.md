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
#> IGRAPH 86cb149 U--- 30 70 -- 
#> + attr: grp (v/c), sign (e/n)
#> + edges from 86cb149:
#>  [1]  1-- 2  1-- 3  1-- 4  1-- 6  1-- 7  1-- 9  2-- 3  2-- 4  2-- 5  2-- 7
#> [11]  2-- 9  2--10  3-- 4  3-- 5  3-- 7  3-- 8  4-- 5  4-- 6  4-- 7  4-- 9
#> [21]  4--10  5-- 6  5-- 8  5-- 9  5--10  6-- 7  6--10  7-- 8  7--10  8-- 9
#> [31]  8--10  9--10 11--19 12--13 12--14 12--18 13--14 13--15 13--16 13--17
#> [41] 13--18 13--19 14--17 14--18 14--19 15--18 15--20 16--18 18--20 21--23
#> [51] 21--24 21--25 22--29 22--30 23--24 23--25 23--27 24--25 24--26 24--30
#> [61] 25--26 25--29 26--29 26--30 27--29 27--30 29--30  3--22 19--24 14--29
```
