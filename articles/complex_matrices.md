# Complex Matrices

This vignette describes the use of complex matrices for signed networks
with ambivalent ties.

``` r
library(igraph)
library(signnet)
```

## Representing networks with ambivalent ties

The vignette on [signed two-mode
network](https://schochastics.github.io/signnet/articles/signed_2mode.md)
introduces a third type of tie for signed networks, the ambivalent tie.

``` r
# construct network
el <- matrix(c(1, "a", 1, "b", 1, "c", 2, "a", 2, "b"), ncol = 2, byrow = TRUE)
g <- graph_from_edgelist(el, directed = FALSE)
E(g)$sign <- c(1, 1, -1, 1, -1)
V(g)$type <- c(FALSE, TRUE, TRUE, TRUE, FALSE)

# vertex duplication
gu <- as_unsigned_2mode(g, primary = TRUE)

# project and binarize
pu <- bipartite_projection(gu, which = "true")
pu <- delete_edge_attr(pu, "weight")

# vertex contraction
ps <- as_signed_proj(pu)
igraph::as_data_frame(ps, "edges")
#>   from to type
#> 1    a  b    A
#> 2    a  c    N
#> 3    b  c    N
```

Ambivalent ties add a new level of complexity for analytic tasks
(especially involving matrices) since it is not clear which value to
assign to them. Intuitively they should be “somewhere” between a
positive and a negative tie but zero is already taken for the null tie.

We can construct a kind of adjacency matrix with the character values,
but we can’t really work with characters analytically.

This is where complex matrices come in. Instead of thinking about edge
values being only in one dimension, we can add a second one for negative
ties. That is, a positive tie would be coded as $(1,0)$ and a negative
one as $(0,1)$. It is much easier in this case to include ambivalent
ties by assigning $(0.5,0.5)$ to them.

Tuples like these can also be written as a complex number, i.e. $(1,0)$
turns into $1 + 0i$, $(0,1)$ into $0 + 1i$, and $(0.5,0.5)$ into
$0.5 + 0.5i$. Complex numbers may be scary to some, but they have a kind
of intuitive interpretation here. The real part is the positive value of
an edge and the imaginary part is the negative part. So we could
actually also have something like $0.3 + 0.7i$ which is an edge that is
30% positive and 70% negative. For now, though, the three values from
above suffice.

The function
[`as_adj_complex()`](https://schochastics.github.io/signnet/reference/as_adj_complex.md)
can be used to return the complex adjacency matrix of a signed network
with ambivalent ties.

``` r
as_adj_complex(ps, attr = "type")
```

    #>          a        b    c
    #> a 0.0+0.0i 0.5+0.5i 0+1i
    #> b 0.5-0.5i 0.0+0.0i 0+1i
    #> c 0.0-1.0i 0.0-1.0i 0+0i

When there is a complex adjacency matrix, then there is also a complex
Laplacian matrix. This matrix can be obtained with
[`laplacian_matrix_complex()`](https://schochastics.github.io/signnet/reference/laplacian_matrix_complex.md).

``` r
laplacian_matrix_complex(ps, attr = "type")
```

    #>                a              b    c
    #> a  1.707107+0.0i -0.500000-0.5i 0-1i
    #> b -0.500000+0.5i  1.707107+0.0i 0-1i
    #> c  0.000000+1.0i  0.000000+1.0i 2+0i

## Functions supporting ambivalent ties

So far, only the triangle routines support networks with ambivalent
ties.

``` r
g <- make_full_graph(5)
E(g)$type <- c(rep("P", 3), rep("N", 3), rep("A", 4))

count_complex_triangles(g, attr = "type")
#> PPP PPN PNN NNN PPA PNA NNA PAA NAA AAA 
#>   0   2   0   0   1   3   1   0   2   1
```
