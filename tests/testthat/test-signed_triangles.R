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

test_that("signed triangle listing works",{
  g <- igraph::graph.full(4)
  igraph::E(g)$sign <- c(1,1,1,-1,-1,-1)
  res_code <- signed_triangles(g)
  res_true <- structure(c(1, 1, 2, 4, 5, 4, 6, 6, 3, 2, 3, 5, 2, 2, 2, 0),
                        .Dim = c(4L, 4L),
                        .Dimnames = list(NULL, c("V1", "V2", "V3", "P")))
  expect_equal(res_code,res_true)
})

test_that("complex triangle count works",{
  g <- igraph::graph.full(4)
  igraph::E(g)$type <- c("P","N","A","A","P","N")
  res <- count_complex_triangles(g,attr = "type")
  res_true <- c(PPP = 0, PPN = 0, PNN = 0, NNN = 0, PPA = 1, PNA = 2, NNA = 1,
                PAA = 0, NAA = 0, AAA = 0)
  expect_equal(res,res_true)
})

test_that("no complex triangles works", {
  g <- igraph::graph.star(5,"undirected")
  igraph::E(g)$type <- "P"
  expect_warning(count_complex_triangles(g,attr = "type"))
})

test_that("complex directed check works", {
  g <- igraph::graph.full(5,directed = T)
  igraph::E(g)$type <- "P"
  expect_error(count_complex_triangles(g,attr = "type"))
})

test_that("complex check works", {
  g <- igraph::graph.full(5,directed = F)
  expect_error(count_complex_triangles(g))
})

test_that("wrong complex values check works", {
  g <- igraph::graph.full(5,directed = F)
  igraph::E(g)$type <- "L"
  expect_error(count_complex_triangles(g,attr = "type"))
})
