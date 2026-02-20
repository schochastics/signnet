# Create a signed graph from an edgelist matrix

Create a signed graph from an edgelist matrix

## Usage

``` r
graph_from_edgelist_signed(el, signs, directed = FALSE)
```

## Arguments

- el:

  The edgelist, a two column matrix, character or numeric.

- signs:

  vector indicating the sign of edges. Entries must be 1 or -1.

- directed:

  whether to create a directed graph.

## Value

a signed network as igraph object

## Examples

``` r
el <- matrix(c("foo", "bar", "bar", "foobar"), ncol = 2, byrow = TRUE)
signs <- c(-1, 1)
graph_from_edgelist_signed(el, signs)
#> IGRAPH b36ef3a UN-- 3 2 -- 
#> + attr: name (v/c), sign (e/n)
#> + edges from b36ef3a (vertex names):
#> [1] foo--bar    bar--foobar
```
