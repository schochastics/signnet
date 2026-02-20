# Changelog

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
