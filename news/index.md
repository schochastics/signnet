# Changelog

## signnet (development version)

### Bug fixes

- [`as_adj_complex()`](https://schochastics.github.io/signnet/reference/as_adj_complex.md)
  now respects the `attr` argument and works with named vertices. This
  also fixes
  [`laplacian_matrix_complex()`](https://schochastics.github.io/signnet/reference/laplacian_matrix_complex.md),
  [`as_incidence_complex()`](https://schochastics.github.io/signnet/reference/as_incidence_complex.md)
  and
  [`complex_walks()`](https://schochastics.github.io/signnet/reference/complex_walks.md)
  for attributes other than `"type"`.
- `laplacian_matrix_signed(sparse = TRUE)` no longer errors. Normalized
  Laplacians (signed and complex) no longer return `NaN` for isolated
  vertices.
- [`sample_islands_signed()`](https://schochastics.github.io/signnet/reference/sample_islands_signed.md)
  always returns `islands.n * islands.size` vertices (previously failed
  when the highest-numbered vertices had no edges).
- `balance_score(method = "walk")` no longer overflows to `NaN` on dense
  networks. `balance_score(method = "triangles")` returns `NA` for
  networks without triangles.
- [`triad_census_signed()`](https://schochastics.github.io/signnet/reference/triad_census_signed.md)
  returns the census in canonical order.
- [`eigen_centrality_signed()`](https://schochastics.github.io/signnet/reference/eigen_centrality_signed.md)
  returns a real vector for directed networks (or errors if the dominant
  eigenvalue is complex).
- [`ggblock()`](https://schochastics.github.io/signnet/reference/ggblock.md)
  colors ties correctly if only one sign is present.
  `ggsigned(type = "complex")` now uses `attr` for edge colors and no
  longer overwrites the `type` attribute.
- [`as_signed_proj()`](https://schochastics.github.io/signnet/reference/as_signed_proj.md)
  handles vertex names containing `"-"`, `"pos"` or `"neg"`.
- [`complex_walks()`](https://schochastics.github.io/signnet/reference/complex_walks.md)
  validates `k`.
- [`signed_blockmodel_general()`](https://schochastics.github.io/signnet/reference/signed_blockmodel_general.md)
  returned a membership that did not match the reported criterion, and a
  wrong (even negative) criterion for `alpha != 0.5`. The meaning of
  `alpha` now matches
  [`signed_blockmodel()`](https://schochastics.github.io/signnet/reference/signed_blockmodel.md).
- `signed_blockmodel(annealing = FALSE)` no longer returns the random
  initial partition for networks with large blocks, and it always takes
  the best improving move.
- [`signed_blockmodel()`](https://schochastics.github.io/signnet/reference/signed_blockmodel.md)
  and
  [`signed_blockmodel_general()`](https://schochastics.github.io/signnet/reference/signed_blockmodel_general.md)
  validate `k`, `alpha` and `blockmat` (square, symmetric for undirected
  networks).

### Maintenance

- Signed adjacency and incidence matrices are built internally instead
  of using the deprecated `attr` argument of igraph. Values of multiple
  edges are still summed.
- Shared input validation. Errors no longer include the internal call.
- Triad census lookup tables moved from the R source to internal data
  (`data-raw/triad_tables.R`).
- Removed unused C++ code and added `src/Makevars`.
- [`sample_gnp_signed()`](https://schochastics.github.io/signnet/reference/sample_gnp_signed.md)
  and
  [`sample_bipartite_signed()`](https://schochastics.github.io/signnet/reference/sample_bipartite_signed.md)
  validate `p_neg`.
- Added examples for
  [`ggblock()`](https://schochastics.github.io/signnet/reference/ggblock.md),
  [`ggsigned()`](https://schochastics.github.io/signnet/reference/ggsigned.md)
  and
  [`frustration_exact()`](https://schochastics.github.io/signnet/reference/frustration_exact.md).
- Requires R \>= 3.5.

### Performance

- [`count_signed_triangles()`](https://schochastics.github.io/signnet/reference/count_signed_triangles.md),
  [`signed_triangles()`](https://schochastics.github.io/signnet/reference/signed_triangles.md)
  and
  [`count_complex_triangles()`](https://schochastics.github.io/signnet/reference/count_complex_triangles.md)
  are vectorized (several hundred times faster on networks with many
  triangles).
- [`as_incidence_complex()`](https://schochastics.github.io/signnet/reference/as_incidence_complex.md)
  and
  [`degree_signed()`](https://schochastics.github.io/signnet/reference/degree_signed.md)
  are vectorized and use sparse matrices.
  [`complex_walks()`](https://schochastics.github.io/signnet/reference/complex_walks.md)
  is about twice as fast.
- [`graph_circular_signed()`](https://schochastics.github.io/signnet/reference/graph_circular_signed.md)
  computes arc lengths directly from the sampled angles, which also
  avoids `NaN`s from rounding errors.
- Both blockmodeling functions share a new C++ implementation (greedy
  local search and simulated annealing) that no longer copies the
  adjacency matrix for every move. `signed_blockmodel(annealing = TRUE)`
  no longer uses [`stats::optim()`](https://rdrr.io/r/stats/optim.html)
  and is about 25x faster.

## signnet 1.0.6

CRAN release: 2025-11-06

- fixed a bug in eigenvector centrality calculation

## signnet 1.0.5

CRAN release: 2025-02-05

- fix all deprecated igraph calls

## signnet 1.0.4

CRAN release: 2024-01-08

- fix deprecated support of adjacency matrices with character values
  [\#26](https://github.com/schochastics/signnet/issues/26)

## signnet 1.0.3

CRAN release: 2023-12-15

- code refactoring
- more tests [\#17](https://github.com/schochastics/signnet/issues/17)
- removed deprecated calls to aes\_
  [\#23](https://github.com/schochastics/signnet/issues/23)

## signnet 1.0.2

CRAN release: 2023-09-08

- fixed a bug in
  [`signed_triangles()`](https://schochastics.github.io/signnet/reference/signed_triangles.md)
  that resulted in wrong vertex ids
  ([\#20](https://github.com/schochastics/signnet/issues/20))

## signnet 1.0.1

CRAN release: 2023-01-27

- fixed an error which occurs with the new version of igraph
  (<https://github.com/igraph/rigraph/pull/633>)

## signnet 1.0.0

CRAN release: 2022-12-22

- added code of conduct
- added contributing guide
- added
  [`frustration_exact()`](https://schochastics.github.io/signnet/reference/frustration_exact.md)
  to vignette
- added utility functions
  `is_signed`,`graph_from_adjacency_matrix_signed`, and
  [`graph_from_edgelist_signed()`](https://schochastics.github.io/signnet/reference/graph_from_edgelist_signed.md)
- added random graph models
  [`sample_gnp_signed()`](https://schochastics.github.io/signnet/reference/sample_gnp_signed.md),
  [`sample_bipartite_signed()`](https://schochastics.github.io/signnet/reference/sample_bipartite_signed.md)

## signnet 0.8.1

CRAN release: 2022-10-18

- fixed existing check errors

## signnet 0.8.0

CRAN release: 2022-02-13

- added
  [`frustration_exact()`](https://schochastics.github.io/signnet/reference/frustration_exact.md)
  to compute the exact number of frustrated edges
- fixed issue with aggregate on r-devel

## signnet 0.7.1

CRAN release: 2021-04-28

- fixed [\#7](https://github.com/schochastics/signnet/issues/7)
- fixed copy paste error in
  [`as_unsigned_2mode()`](https://schochastics.github.io/signnet/reference/as_unsigned_2mode.md)
- fixed aggregate error in
  [`as_signed_proj()`](https://schochastics.github.io/signnet/reference/as_signed_proj.md)

## signnet 0.7.0

CRAN release: 2020-10-21

- added
  [`triad_census_signed()`](https://schochastics.github.io/signnet/reference/triad_census_signed.md)

## signnet 0.6.0

CRAN release: 2020-08-13

- added `avatar` dataset
- speed up of blockmodeling for larger networks

## signnet 0.5.3

CRAN release: 2020-06-30

- fixed issue in
  [`complex_walks()`](https://schochastics.github.io/signnet/reference/complex_walks.md)
- fixed faulty calculation of directed
  [`pn_index()`](https://schochastics.github.io/signnet/reference/pn_index.md)

## signnet 0.5.2

CRAN release: 2020-03-04

- fixed `stringsAsFactors` issue in `complex_matrices.R`

## signnet 0.5.1

CRAN release: 2020-02-04

- fixed C++ issue for circular arc graphs
- fixed failing eigen centrality test

## signnet 0.5.0

CRAN release: 2020-01-24

- added vignettes and tests

## signnet 0.1.0

- initial version
