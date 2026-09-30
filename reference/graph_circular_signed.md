# circular signed graph

circular graph with positive and negative edges.

## Usage

``` r
graph_circular_signed(n, r = 1, pos = 0.1, neg = 0.1)
```

## Arguments

- n:

  number of nodes

- r:

  radius

- pos:

  distance fraction between positive edges

- neg:

  distance fraction between negative edges

## Value

igraph graph

## Author

David Schoch

## Examples

``` r
library(igraph)
graph_circular_signed(n = 50)
#> IGRAPH d819fb8 U--- 50 373 -- 
#> + attr: x (v/n), y (v/n), sign (e/n)
#> + edges from d819fb8:
#>   [1] 1-- 3 1-- 9 1--13 1--18 1--19 1--25 1--27 1--34 1--36 1--40 1--43 2-- 4
#>  [13] 2-- 6 2-- 7 2--12 2--13 2--16 2--17 2--22 2--26 2--28 2--35 2--38 2--39
#>  [25] 2--43 2--48 2--50 3-- 5 3-- 8 3--17 3--18 3--21 3--22 3--24 3--25 3--27
#>  [37] 3--34 3--35 3--36 3--40 3--43 3--49 4-- 6 4-- 7 4-- 9 4--10 4--11 4--12
#>  [49] 4--13 4--15 4--16 4--17 4--20 4--22 4--28 4--38 4--39 4--41 4--44 4--48
#>  [61] 4--50 5-- 8 5--18 5--19 5--21 5--24 5--25 5--27 5--34 5--36 5--43 5--46
#>  [73] 5--49 6-- 7 6-- 9 6--10 6--12 6--13 6--15 6--16 6--17 6--20 6--22 6--28
#>  [85] 6--38 6--39 6--41 6--44 6--48 6--50 7--12 7--13 7--15 7--16 7--17 7--20
#> + ... omitted several edges
```
