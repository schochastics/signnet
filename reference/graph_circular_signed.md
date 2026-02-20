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
#> IGRAPH daf8a9c U--- 50 360 -- 
#> + attr: x (v/n), y (v/n), sign (e/n)
#> + edges from daf8a9c:
#>   [1] 1-- 2 1-- 4 1-- 5 1-- 6 1-- 9 1--11 1--17 1--24 1--27 1--28 1--29 1--30
#>  [13] 1--33 1--37 1--39 1--43 1--48 1--49 2-- 4 2-- 6 2-- 9 2--11 2--17 2--24
#>  [25] 2--27 2--28 2--29 2--30 2--33 2--37 2--39 2--42 2--43 2--46 2--48 2--49
#>  [37] 3-- 4 3--18 3--19 3--20 3--21 3--22 3--26 3--33 3--34 3--36 3--40 3--41
#>  [49] 3--45 4-- 5 4-- 9 4--15 4--17 4--20 4--27 4--28 4--30 4--33 4--37 4--39
#>  [61] 4--41 4--48 4--49 5-- 6 5-- 9 5--11 5--15 5--17 5--22 5--24 5--27 5--28
#>  [73] 5--30 5--33 5--37 5--39 5--40 5--42 5--43 5--48 5--49 6--11 6--15 6--17
#>  [85] 6--24 6--27 6--28 6--32 6--37 6--39 6--42 6--43 6--46 6--48 6--49 6--50
#> + ... omitted several edges
```
