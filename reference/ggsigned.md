# Plot a signed or complex network

Plot a signed or complex network

## Usage

``` r
ggsigned(g, type = "signed", attr = NULL, edge_cols = NULL, weights = FALSE)
```

## Arguments

- g:

  igraph object. Must have a "sign" edge attribute or an attribute
  containing "P", "N", "A"

- type:

  character string. either "signed" or "complex"

- attr:

  character string. edge attribute that containing "P", "N", "A" if
  type="complex"

- edge_cols:

  colors used for negative and positive (and ambivalent) ties

- weights:

  logical. If TRUE, weights are computed based on sign. Defaults to
  FALSE

## Value

ggplot2 object

## Details

This is a very rudimentary visualization of a signed network. If you are
fluent in 'ggraph', you can probably cook up something more
sophisticated. The function is thus mostly meant to give a quick
overview of the network.

## Author

David Schoch
