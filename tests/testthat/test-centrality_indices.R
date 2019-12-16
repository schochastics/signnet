test_that("pos degree works", {
  g <- igraph::graph.full(4)
  igraph::E(g)$sign <- c(-1,1,1,-1,-1,1)
  expect_equal(degree_signed(g,mode="all",type="pos"),c(2,0,2,2))
})

test_that("neg degree works", {
  g <- igraph::graph.full(4)
  igraph::E(g)$sign <- c(-1,1,1,-1,-1,1)
  expect_equal(degree_signed(g,mode="all",type="neg"),c(1,3,1,1))
})

test_that("pos degree works", {
  g <- igraph::graph.full(4)
  igraph::E(g)$sign <- c(-1,1,1,-1,-1,1)
  expect_equal(degree_signed(g,mode="all",type="pos"),c(2,0,2,2))
})

test_that("net in degree works", {
  g <- igraph::graph.full(4,directed = TRUE)
  igraph::E(g)$sign <- rep(c(-1,1,1,-1,-1,1),2)
  expect_equal(degree_signed(g,mode="in",type="net"),c(-3,-1,1,3))
})

test_that("net out degree works", {
  g <- igraph::graph.full(4,directed = TRUE)
  igraph::E(g)$sign <- rep(c(-1,1,1,-1,-1,1),2)
  expect_equal(degree_signed(g,mode="out",type="net"),c(1,-1,1,-1))
})
