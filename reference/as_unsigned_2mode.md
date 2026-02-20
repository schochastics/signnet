# convert signed two-mode network to unsigned

convert signed two-mode network to unsigned

## Usage

``` r
as_unsigned_2mode(g, primary = TRUE)
```

## Arguments

- g:

  igraph object. Two-mode network, must have a "sign" edge attribute.

- primary:

  logical. Which mode to transform

## Value

igraph object

## See also

[as_signed_proj](https://schochastics.github.io/signnet/reference/as_signed_proj.md)

## Author

David Schoch

## Examples

``` r
library(igraph)

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
#> IGRAPH 3d56c71 UN-- 3 3 -- 
#> + attr: name (v/c), type (e/c)
#> + edges from 3d56c71 (vertex names):
#> [1] a--b a--c b--c
```
