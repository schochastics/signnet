# convert unsigned projection to signed

convert unsigned projection to signed

## Usage

``` r
as_signed_proj(g)
```

## Arguments

- g:

  igraph object

## Value

igraph object

## See also

[as_unsigned_2mode](https://schochastics.github.io/signnet/reference/as_unsigned_2mode.md)

## Author

David Schoch

## Examples

``` r
library(igraph)
#> 
#> Attaching package: ‘igraph’
#> The following objects are masked from ‘package:stats’:
#> 
#>     decompose, spectrum
#> The following object is masked from ‘package:base’:
#> 
#>     union

# create a simple signed two mode network
el <- matrix(c(1, "a", 1, "b", 1, "c", 2, "a", 2, "b"), ncol = 2, byrow = TRUE)
g <- graph_from_edgelist(el, directed = FALSE)
E(g)$sign <- c(1, 1, -1, 1, -1)
V(g)$type <- c(FALSE, TRUE, TRUE, TRUE, FALSE)

# convert to unsigned two-mode network and project
l <- as_unsigned_2mode(g, primary = TRUE)
p <- bipartite_projection(l, which = "true")

# turn the unsigned projection back to a signed network
as_signed_proj(p)
#> IGRAPH 1afdbbc UN-- 3 3 -- 
#> + attr: name (v/c), type (e/c)
#> + edges from 1afdbbc (vertex names):
#> [1] a--b a--c b--c
```
