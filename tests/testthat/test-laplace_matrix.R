test_that("laplacian matrix correct", {
  g <- igraph::graph.full(5)
  igraph::E(g)$sign <- -1
  L <- laplacian_matrix_signed(g)
  expect_equal(diag(L), rep(4,5))
})
