test_that("degree error no graph works",{
  expect_error(degree_signed(g=5))
})

test_that("degree error no sign works",{
  g <- igraph::graph.full(5,FALSE)
  expect_error(degree_signed(g))
})

test_that("degree error directed works",{
  g <- igraph::graph.full(5,TRUE)
  igraph::E(g)$sign <- 1
  expect_error(degree_signed(g,mode="all"))
})

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

test_that("pn index error no graph works",{
  expect_error(pn_index(g=5))
})

test_that("pn index error no sign works",{
  g <- igraph::graph.full(5,FALSE)
  expect_error(pn_index(g))
})

test_that("pn index error directed works",{
  g <- igraph::graph.full(5,TRUE)
  igraph::E(g)$sign <- 1
  expect_error(pn_index(g,mode="all"))
})

test_that("pn index works",{
  g <- igraph::graph.full(5,FALSE)
  igraph::E(g)$sign <- 1
  expect_equal(pn_index(g,mode="in"),c(2, 2, 2, 2, 2))
})
