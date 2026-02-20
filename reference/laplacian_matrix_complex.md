# Complex Graph Laplacian

The Laplacian of a signed graph containing ambivalent ties.

## Usage

``` r
laplacian_matrix_complex(g, attr, norm = FALSE)
```

## Arguments

- g:

  igraph object.

- attr:

  edge attribute name that encodes positive ("P"), negative ("N") and
  ambivalent ("A") ties.

- norm:

  Whether to calculate the normalized Laplacian. See definitions below.

## Value

a complex matrix

## Details

See
[laplacian_matrix](https://r.igraph.org/reference/laplacian_matrix.html)
of igraph for more details. In the complex case, D is a diagonal matrix
containing the absolute values of row sums of the complex adjacency
matrix.

## See also

[laplacian_matrix_signed](https://schochastics.github.io/signnet/reference/laplacian_matrix_signed.md)

## Author

David Schoch
