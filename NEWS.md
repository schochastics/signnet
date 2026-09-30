# signnet (development version)

## Bug fixes

* `as_adj_complex()` now respects the `attr` argument and works with named vertices. This also fixes `laplacian_matrix_complex()`, `as_incidence_complex()` and `complex_walks()` for attributes other than `"type"`.
* `laplacian_matrix_signed(sparse = TRUE)` no longer errors. Normalized Laplacians (signed and complex) no longer return `NaN` for isolated vertices.
* `sample_islands_signed()` always returns `islands.n * islands.size` vertices (previously failed when the highest-numbered vertices had no edges).
* `balance_score(method = "walk")` no longer overflows to `NaN` on dense networks. `balance_score(method = "triangles")` returns `NA` for networks without triangles.
* `triad_census_signed()` returns the census in canonical order.
* `eigen_centrality_signed()` returns a real vector for directed networks (or errors if the dominant eigenvalue is complex).
* `ggblock()` colors ties correctly if only one sign is present. `ggsigned(type = "complex")` now uses `attr` for edge colors and no longer overwrites the `type` attribute.
* `as_signed_proj()` handles vertex names containing `"-"`, `"pos"` or `"neg"`.
* `complex_walks()` validates `k`.
* `signed_blockmodel_general()` returned a membership that did not match the reported criterion, and a wrong (even negative) criterion for `alpha != 0.5`. The meaning of `alpha` now matches `signed_blockmodel()`.
* `signed_blockmodel(annealing = FALSE)` no longer returns the random initial partition for networks with large blocks, and it always takes the best improving move.
* `signed_blockmodel()` and `signed_blockmodel_general()` validate `k`, `alpha` and `blockmat` (square, symmetric for undirected networks).

## Performance

* `count_signed_triangles()`, `signed_triangles()` and `count_complex_triangles()` are vectorized (several hundred times faster on networks with many triangles).
* `as_incidence_complex()` and `degree_signed()` are vectorized and use sparse matrices. `complex_walks()` is about twice as fast.
* `graph_circular_signed()` computes arc lengths directly from the sampled angles, which also avoids `NaN`s from rounding errors.
* Both blockmodeling functions share a new C++ implementation (greedy local search and simulated annealing) that no longer copies the adjacency matrix for every move. `signed_blockmodel(annealing = TRUE)` no longer uses `stats::optim()` and is about 25x faster.

# signnet 1.0.6

* fixed a bug in eigenvector centrality calculation

# signnet 1.0.5

* fix all deprecated igraph calls

# signnet 1.0.4

* fix deprecated support of adjacency matrices with character values #26

# signnet 1.0.3

* code refactoring
* more tests #17
* removed deprecated calls to aes_ #23

# signnet 1.0.2

* fixed a bug in `signed_triangles()` that resulted in wrong vertex ids (#20)

# signnet 1.0.1

* fixed an error which occurs with the new version of igraph (https://github.com/igraph/rigraph/pull/633)

# signnet 1.0.0

* added code of conduct
* added contributing guide
* added `frustration_exact()` to vignette
* added utility functions `is_signed`,`graph_from_adjacency_matrix_signed`, and `graph_from_edgelist_signed()`
* added random graph models `sample_gnp_signed()`, `sample_bipartite_signed()`

# signnet 0.8.1

* fixed existing check errors

# signnet 0.8.0

* added `frustration_exact()` to compute the exact number of frustrated edges
* fixed issue with aggregate on r-devel

# signnet 0.7.1

* fixed #7
* fixed copy paste error in `as_unsigned_2mode()`
* fixed aggregate error in `as_signed_proj()`

# signnet 0.7.0

* added `triad_census_signed()`

# signnet 0.6.0

* added `avatar` dataset
* speed up of blockmodeling for larger networks

# signnet 0.5.3

* fixed issue in `complex_walks()`
* fixed faulty calculation of directed `pn_index()`

# signnet 0.5.2

* fixed `stringsAsFactors` issue in `complex_matrices.R`

# signnet 0.5.1

* fixed C++ issue for circular arc graphs
* fixed failing eigen centrality test

# signnet 0.5.0

* added vignettes and tests

# signnet 0.1.0

* initial version


