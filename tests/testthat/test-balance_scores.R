test_that("triangle balance index works", {
  g <- igraph::graph.full(5)
  igraph::E(g)$sign <- 1
  expect_equal(balance_score(g,method="triangles"), 1)
})

test_that("walk balance index works", {
  g <- igraph::graph.full(5)
  igraph::E(g)$sign <- 1
  expect_equal(balance_score(g,method="walk"), 1)
})

test_that("frustration balance index works", {
  g <- igraph::graph.full(5)
  igraph::E(g)$sign <- 1
  expect_equal(balance_score(g,method="frustration"), 1)
})


test_that("directed check works", {
  g <- igraph::graph.full(5,directed = T)
  igraph::E(g)$sign <- 1
  expect_error(balance_score(g))
})

test_that("sign check works", {
  g <- igraph::graph.full(5,directed = F)
  expect_error(balance_score(g))
})

test_that("wrong sign values check works", {
  g <- igraph::graph.full(5,directed = F)
  igraph::E(g)$sign <- 2
  expect_error(balance_score(g))
})
