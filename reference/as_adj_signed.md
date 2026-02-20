# Convert a signed graph to a signed adjacency matrix

This function returns the adjacency matrix for a signed graph

## Usage

``` r
as_adj_signed(g, sparse = FALSE)
```

## Arguments

- g:

  igraph object. Must have a "sign" edge attribute.

- sparse:

  Logical scalar, whether to return the result as a sparse matrix. The
  Matrix package is required for sparse matrices.

## Value

signed adjacency matrix

## See also

[as_adj_complex](https://schochastics.github.io/signnet/reference/as_adj_complex.md)
