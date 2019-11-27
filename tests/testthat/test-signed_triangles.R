test_that("only positive works", {
  g <- igraph::graph.full(3)
  igraph::E(g)$sign <- 1
  res_code <- count_signed_triangles(g)
  res_true <- c("+++" = 1,"++-" = 0,"+--" = 0,"---" = 0)
  expect_equal(res_code, res_true)
})

test_that("no triangles works", {
  g <- igraph::graph.star(5,"undirected")
  igraph::E(g)$sign <- 1
  expect_warning(count_signed_triangles(g))
})

test_that("directed check works", {
  g <- igraph::graph.full(5,directed = T)
  igraph::E(g)$sign <- 1
  expect_error(count_signed_triangles(g))
})

test_that("sign check works", {
  g <- igraph::graph.full(5,directed = F)
  expect_error(count_signed_triangles(g))
})

test_that("wrong sign values check works", {
  g <- igraph::graph.full(5,directed = F)
  igraph::E(g)$sign <- 2
  expect_error(count_signed_triangles(g))
})



test_that("no triangles works", {
  g <- igraph::graph.star(5,"undirected")
  igraph::E(g)$sign <- 1
  expect_warning(signed_triangles(g))
})

test_that("directed check works", {
  g <- igraph::graph.full(5,directed = T)
  igraph::E(g)$sign <- 1
  expect_error(signed_triangles(g))
})

test_that("sign check works", {
  g <- igraph::graph.full(5,directed = F)
  expect_error(signed_triangles(g))
})

test_that("wrong sign values check works", {
  g <- igraph::graph.full(5,directed = F)
  igraph::E(g)$sign <- 2
  expect_error(signed_triangles(g))
})
