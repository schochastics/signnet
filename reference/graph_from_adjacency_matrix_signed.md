# Create signed graphs from adjacency matrices

Create signed graphs from adjacency matrices

## Usage

``` r
graph_from_adjacency_matrix_signed(A, mode = "undirected", ...)
```

## Arguments

- A:

  square adjacency matrix of a signed graph

- mode:

  Character scalar, specifies how to interpret the supplied matrix.
  Possible values are: directed, undirected

- ...:

  additional parameters for
  [`from_adjacency()`](https://r.igraph.org/reference/graph_from_adjacency_matrix.html)

## Value

a signed network as igraph object

## Examples

``` r
A <- matrix(c(0, 1, -1, 1, 0, 1, -1, 1, 0), 3, 3)
graph_from_adjacency_matrix_signed(A)
#> IGRAPH 204bccd U--- 3 3 -- 
#> + attr: sign (e/n)
#> + edges from 204bccd:
#> [1] 1--2 1--3 2--3
```
