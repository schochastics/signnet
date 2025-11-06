test_that("degree error no graph works", {
  expect_error(degree_signed(g = 5))
})

test_that("degree error no sign works", {
  g <- igraph::make_full_graph(5, FALSE)
  expect_error(degree_signed(g))
})

test_that("degree error directed works", {
  g <- igraph::make_full_graph(5, TRUE)
  igraph::E(g)$sign <- 1
  expect_error(degree_signed(g, mode = "all"))
})

test_that("pos degree works", {
  g <- igraph::make_full_graph(4)
  igraph::E(g)$sign <- c(-1, 1, 1, -1, -1, 1)
  expect_equal(degree_signed(g, mode = "all", type = "pos"), c(2, 0, 2, 2))
})

test_that("neg degree works", {
  g <- igraph::make_full_graph(4)
  igraph::E(g)$sign <- c(-1, 1, 1, -1, -1, 1)
  expect_equal(degree_signed(g, mode = "all", type = "neg"), c(1, 3, 1, 1))
})

test_that("pos degree works", {
  g <- igraph::make_full_graph(4)
  igraph::E(g)$sign <- c(-1, 1, 1, -1, -1, 1)
  expect_equal(degree_signed(g, mode = "all", type = "pos"), c(2, 0, 2, 2))
})

test_that("net in degree works", {
  g <- igraph::make_full_graph(4, directed = TRUE)
  igraph::E(g)$sign <- rep(c(-1, 1, 1, -1, -1, 1), 2)
  expect_equal(degree_signed(g, mode = "in", type = "net"), c(-3, -1, 1, 3))
})

test_that("net out degree works", {
  g <- igraph::make_full_graph(4, directed = TRUE)
  igraph::E(g)$sign <- rep(c(-1, 1, 1, -1, -1, 1), 2)
  expect_equal(degree_signed(g, mode = "out", type = "net"), c(1, -1, 1, -1))
})

test_that("pn index error no graph works", {
  expect_error(pn_index(g = 5))
})

test_that("pn index error no sign works", {
  g <- igraph::make_full_graph(5, FALSE)
  expect_error(pn_index(g))
})

test_that("pn index error directed works", {
  g <- igraph::make_full_graph(5, TRUE)
  igraph::E(g)$sign <- 1
  expect_error(pn_index(g, mode = "all"))
})

test_that("pn index works", {
  g <- igraph::make_full_graph(5, FALSE)
  igraph::E(g)$sign <- 1
  expect_equal(pn_index(g, mode = "in"), c(2, 2, 2, 2, 2))
})

test_that("evcent not graph error works", {
  expect_error(eigen_centrality_signed(g = 5))
})

test_that("evcent no sign error works", {
  g <- igraph::make_full_graph(5)
  expect_error(eigen_centrality_signed(g))
})

test_that("evcent not dominant works", {
  A <- matrix(
    c(
      0,
      1,
      1,
      -1,
      0,
      0,
      -1,
      0,
      0,
      1,
      0,
      1,
      0,
      -1,
      0,
      0,
      -1,
      0,
      1,
      1,
      0,
      0,
      0,
      -1,
      0,
      0,
      -1,
      -1,
      0,
      0,
      0,
      1,
      1,
      -1,
      0,
      0,
      0,
      -1,
      0,
      1,
      0,
      1,
      0,
      -1,
      0,
      0,
      0,
      -1,
      1,
      1,
      0,
      0,
      0,
      -1,
      -1,
      0,
      0,
      -1,
      0,
      0,
      0,
      1,
      1,
      0,
      -1,
      0,
      0,
      -1,
      0,
      1,
      0,
      1,
      0,
      0,
      -1,
      0,
      0,
      -1,
      1,
      1,
      0
    ),
    9,
    9
  )
  g <- graph_from_adjacency_matrix_signed(A, "undirected")
  expect_error(eigen_centrality_signed(g))
})

test_that("evcent works", {
  g <- igraph::make_full_graph(5)
  igraph::E(g)$sign <- 1
  igraph::E(g)$sign[1] <- -1
  ev <- abs(round(eigen_centrality_signed(g, scale = TRUE), 8))
  ev_true <- c(0.68614066, 0.68614066, 1, 1, 1)
  expect_equal(stats::cor(ev, ev_true), 1)
})

test_that("evcent no scale works", {
  g <- igraph::make_full_graph(5)
  igraph::E(g)$sign <- 1
  igraph::E(g)$sign[1] <- -1
  ev <- abs(round(eigen_centrality_signed(g, scale = FALSE), 8))
  ev_true <- c(0.34560347, 0.34560347, 0.50369186, 0.50369186, 0.50369186)
  expect_equal(stats::cor(ev, ev_true), 1)
})
