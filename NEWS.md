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


