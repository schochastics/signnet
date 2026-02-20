# Complex Incidence Matrix

The complex incidence matrix of a signed graph containing ambivalent
ties.

## Usage

``` r
as_incidence_complex(g, attr)
```

## Arguments

- g:

  igraph object.

- attr:

  edge attribute name that encodes positive ("P"), negative ("N") and
  ambivalent ("A") ties.

## Value

a complex matrix

## Details

This function is slightly different than
[as_incidence_matrix](https://r.igraph.org/reference/as_incidence_matrix.html)
since it is defined for bipartite graphs. The incidence matrix here is
defined as a \\S \in C^{n,m}\\, where n is the number of vertices and m
the number of edges. Edges (i,j) are oriented such that i\<j and entries
are defined as \$\$S\_{i(i,j)}=\sqrt{A\_{ij}}\$\$
\$\$S\_{j(i,j)}=-\sqrt{A\_{ji}} if (i,j) is an ambivalent tie\$\$
\$\$S\_{j(i,j)}=-A\_{ji}\sqrt{A\_{ji}} else\$\$

## See also

[laplacian_matrix_complex](https://schochastics.github.io/signnet/reference/laplacian_matrix_complex.md),[as_adj_complex](https://schochastics.github.io/signnet/reference/as_adj_complex.md)

## Author

David Schoch
