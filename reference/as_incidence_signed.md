# Convert a signed two-mode network to a signed matrix

This function returns the incidence matrix for a signed two-mode
network.

## Usage

``` r
as_incidence_signed(g, sparse = FALSE)
```

## Arguments

- g:

  igraph object (bipartite). Must have a "sign" edge attribute.

- sparse:

  Logical scalar, whether to return the result as a sparse matrix. The
  Matrix package is required for sparse matrices.

## Value

signed incidence matrix
