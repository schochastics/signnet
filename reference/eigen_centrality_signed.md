# Signed Eigenvector centrality

returns the eigenvector associated with the dominant eigenvalue from the
adjacency matrix.

## Usage

``` r
eigen_centrality_signed(g, scale = TRUE)
```

## Arguments

- g:

  igraph object with a sign edge attribute.

- scale:

  Logical scalar, whether to scale the result to have a maximum score of
  one. If no scaling is used then the result vector is the same as
  returned by [`eigen()`](https://rdrr.io/r/base/eigen.html).

## Value

centrality scores as numeric vector.

## Details

Note that, with negative values, the adjacency matrix may not have a
dominant eigenvalue. This means it is not clear which eigenvector should
be used. In addition it is possible for the adjacency matrix to have
repeated eigenvalues and hence multiple linearly independent
eigenvectors. In this case certain centralities can be arbitrarily
assigned. The function returns an error if this is the case.

## References

Bonacich, P. and Lloyd, P. (2004). "Calculating Status with Negative
Relations." *Social Networks* 26 (4): 331–38.

Everett, M. and Borgatti, S.P. (2014). "Networks Containing Negative
Ties." *Social Networks* 38: 111–20.

## Author

David Schoch

## Examples

``` r
library(igraph)
data("tribes")
eigen_centrality_signed(tribes)
#>  [1] -1.00000000 -0.88879594  0.71609511  0.36818769  0.74303157  0.95454966
#>  [7]  0.76017717  0.67100494  0.21063522  0.23890028  0.69639725  0.91352983
#> [13]  0.23390770  0.05855799 -0.91193923 -0.98724909
```
