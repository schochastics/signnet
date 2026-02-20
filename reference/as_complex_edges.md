# Convert Signed Network to Complex

Convert Signed Network to Complex

## Usage

``` r
as_complex_edges(g, attr = "type")
```

## Arguments

- g:

  igraph object. Must have a "sign" edge attribute.

- attr:

  new edge attribute name that encodes positive ("P"), negative ("N")
  and ambivalent ("A") ties.

## Value

igraph object

## Author

David Schoch

## Examples

``` r
g <- sample_islands_signed(2, 10, 1, 10)
as_complex_edges(g)
#> IGRAPH d46138b U--- 20 110 -- 
#> + attr: grp (v/c), sign (e/n), type (e/c)
#> + edges from d46138b:
#>  [1]  1-- 2  1-- 3  1-- 4  1-- 5  1-- 6  1-- 7  1-- 8  1-- 9  1--10  2-- 3
#> [11]  2-- 4  2-- 5  2-- 6  2-- 7  2-- 8  2-- 9  2--10  3-- 4  3-- 5  3-- 6
#> [21]  3-- 7  3-- 8  3-- 9  3--10  4-- 5  4-- 6  4-- 7  4-- 8  4-- 9  4--10
#> [31]  5-- 6  5-- 7  5-- 8  5-- 9  5--10  6-- 7  6-- 8  6-- 9  6--10  7-- 8
#> [41]  7-- 9  7--10  8-- 9  8--10  9--10 11--12 11--13 11--14 11--15 11--16
#> [51] 11--17 11--18 11--19 11--20 12--13 12--14 12--15 12--16 12--17 12--18
#> [61] 12--19 12--20 13--14 13--15 13--16 13--17 13--18 13--19 13--20 14--15
#> [71] 14--16 14--17 14--18 14--19 14--20 15--16 15--17 15--18 15--19 15--20
#> + ... omitted several edges
```
