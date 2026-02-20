# Convert a signed graph to a complex adjacency matrix

This function returns the adjacency matrix for a signed graph that
contains ambivalent ties

## Usage

``` r
as_adj_complex(g, attr)
```

## Arguments

- g:

  igraph object

- attr:

  edge attribute name that encodes positive ("P"), negative ("N") and
  ambivalent ("A") ties.

## Value

complex adjacency matrix

## See also

[as_adj_signed](https://schochastics.github.io/signnet/reference/as_adj_signed.md)
