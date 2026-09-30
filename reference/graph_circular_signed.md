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
#> IGRAPH 19d2d50 U--- 50 371 -- 
#> + attr: x (v/n), y (v/n), sign (e/n)
#> + edges from 19d2d50:
#>   [1] 1-- 5 1-- 8 1--17 1--19 1--21 1--22 1--23 1--25 1--29 1--30 1--31 1--33
#>  [13] 1--39 1--46 1--47 1--48 1--49 1--50 2-- 5 2--14 2--15 2--18 2--20 2--22
#>  [25] 2--25 2--29 2--33 2--44 2--46 2--47 2--48 2--49 2--50 3-- 9 3--11 3--12
#>  [37] 3--24 3--26 3--28 3--32 3--34 3--35 3--36 3--38 3--40 3--41 3--43 4-- 6
#>  [49] 4--10 4--13 4--14 4--15 4--18 4--20 4--27 4--37 4--42 4--44 5-- 8 5--11
#>  [61] 5--12 5--17 5--19 5--21 5--23 5--24 5--25 5--26 5--29 5--30 5--31 5--34
#>  [73] 5--39 5--40 5--47 5--50 6-- 7 6--10 6--13 6--14 6--15 6--18 6--20 6--27
#>  [85] 6--37 6--42 6--44 6--45 7--13 7--14 7--15 7--18 7--20 7--22 7--27 7--33
#> + ... omitted several edges
```
