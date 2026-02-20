# Plot Blockmodel matrix

Plot Blockmodel matrix

## Usage

``` r
ggblock(
  g,
  blocks = NULL,
  cols = NULL,
  show_blocks = FALSE,
  show_labels = FALSE
)
```

## Arguments

- g:

  igraph object with a sign edge attribute.

- blocks:

  vector of block membership as obtained, e.g. from
  [signed_blockmodel](https://schochastics.github.io/signnet/reference/signed_blockmodel.md)

- cols:

  colors used for negative and positive ties

- show_blocks:

  logical. Should block borders be displayed? (Default: FALSE)

- show_labels:

  logical. Should node labels be displayed? (Default: FALSE)

## Value

ggplot2 object

## Author

David Schoch

## Examples

``` r
if (FALSE) { # \dontrun{
library(igraph)
data("tribes")
clu <- signed_blockmodel(tribes, k = 3, alpha = 0.5, annealing = TRUE)
ggblock(tribes, clu$membership, show_blocks = TRUE, show_labels = TRUE)
} # }
```
