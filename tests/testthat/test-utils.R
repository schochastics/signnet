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
