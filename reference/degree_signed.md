# Signed Degree

several options to calculate the signed degree of vertices

## Usage

``` r
degree_signed(
  g,
  mode = c("all", "in", "out"),
  type = c("pos", "neg", "ratio", "net")
)
```

## Arguments

- g:

  igraph object with a sign edge attribute.

- mode:

  character string, “out” for out-degree, “in” for in-degree or “all”
  for undirected networks.

- type:

  character string, “pos” or “neg” for counting positive or negative
  neighbors only, "ratio" for pos/(pos+neg), or "net" for pos-neg.

## Value

centrality scores as numeric vector.

## Author

David Schoch
