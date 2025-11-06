test_that("signed islands islands.n error works", {
  expect_error(sample_islands_signed(1, 10, 1, 1))
})

test_that("signed islands islands.n error works", {
  expect_error(sample_islands_signed(3, 1, 1, 1))
})

test_that("signed islands islands.pin error works", {
  expect_error(sample_islands_signed(3, 3, 2, 1))
})

test_that("signed islands n.inter error works", {
  expect_error(sample_islands_signed(3, 3, 1, 0))
})

test_that("signed islands works", {
  g <- sample_islands_signed(2, 10, 1, 1)
  expect_lte(igraph::ecount(g), choose(10, 2) * 2 + 2)
})

test_that("circular signed works", {
  g <- graph_circular_signed(10, pos = 1, neg = 0)
  expect_equal(igraph::edge_density(g), 1)
})

test_that("sample signed gnp works", {
  g <- sample_gnp_signed(10, 1, 1)
  expect_equal(igraph::edge_density(g), 1)
})

test_that("sample signed b1partite works", {
  g <- sample_bipartite_signed(10, 10, 1, 1)
  expect_equal(igraph::ecount(g), 100)
})
