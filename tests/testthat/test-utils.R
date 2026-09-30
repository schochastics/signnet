test_that("from_adjacency throws error", {
  A <- matrix(c(0,2,-2,1,0,1,-1,1,0),3,3)
  expect_error(graph_from_adjacency_matrix_signed(A))
})

test_that("from_edgelist works",{
  el <- matrix(c("foo", "bar", "bar", "foobar"), ncol = 2, byrow = TRUE)
  signs <- c(-1, 1)
  g <- graph_from_edgelist_signed(el, signs)
  expect_equal(signs,igraph::E(g)$sign)
  expect_equal(igraph::ecount(g),2)
})

test_that("from_edgelist throws error",{
  el <- matrix(c("foo", "bar", "bar", "foobar"), ncol = 2, byrow = TRUE)
  signs <- c(-1, 2)
  expect_error(graph_from_edgelist_signed(el, signs))
})

test_that("signed adjacency sums multi-edges and counts loops once", {
  g <- igraph::make_graph(c(1, 2, 1, 2, 2, 3, 3, 3), directed = FALSE)
  igraph::E(g)$sign <- c(1, 1, -1, 1)
  A <- as_adj_signed(g)
  expect_equal(A, matrix(c(0, 2, 0, 2, 0, -1, 0, -1, 1), 3, 3))
  d <- igraph::make_graph(c(1, 2, 2, 3), directed = TRUE)
  igraph::E(d)$sign <- c(1, -1)
  igraph::V(d)$name <- c("a", "b", "c")
  A <- as_adj_signed(d, sparse = TRUE)
  expect_s4_class(A, "dgCMatrix")
  expect_equal(dimnames(A), list(c("a", "b", "c"), c("a", "b", "c")))
  expect_equal(A["a", "b"], 1)
  expect_equal(A["b", "a"], 0)
  expect_equal(A["b", "c"], -1)
})

test_that("signed incidence uses vertex ids without names", {
  g <- igraph::make_graph(c(1, 3, 2, 3, 1, 4), directed = FALSE)
  igraph::V(g)$type <- c(FALSE, FALSE, TRUE, TRUE)
  igraph::E(g)$sign <- c(1, -1, -1)
  A <- as_incidence_signed(g)
  expect_equal(dimnames(A), list(c("1", "2"), c("3", "4")))
  expect_equal(unname(A), matrix(c(1, -1, -1, 0), 2, 2))
})

test_that("random signed graphs check p_neg", {
  expect_error(sample_gnp_signed(10, 0.5), "p_neg")
  expect_error(sample_gnp_signed(10, 0.5, 2), "p_neg")
  expect_error(sample_bipartite_signed(5, 5, 0.5), "p_neg")
})
