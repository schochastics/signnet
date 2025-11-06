test_that("blockmodeling works", {
  data("tribes")
  clu <- signed_blockmodel(tribes, k = 3, alpha = 0.5, annealing = TRUE)
  expect_lte(max(clu$membership), 3)
})

test_that("blockmodeling no anneal works ", {
  data("tribes")
  clu <- signed_blockmodel(tribes, k = 3, alpha = 0.5, annealing = FALSE)
  expect_lte(max(clu$membership), 3)
})

test_that("blockmodeling sign check works", {
  g <- igraph::make_full_graph(5)
  expect_error(signed_blockmodel(g))
})

test_that("blockmodeling k error works", {
  data("tribes")
  expect_error(signed_blockmodel(tribes))
})

test_that("general blockmodeling works", {
  data("tribes")
  clu <- signed_blockmodel_general(
    tribes,
    blockmat = matrix(c(1, -1, -1, -1, 1, -1, -1, -1, 1), 3, 3, byrow = T)
  )
  expect_lte(max(clu$membership), 3)
})

test_that("general blockmodeling blockmat error works", {
  data("tribes")
  expect_error(signed_blockmodel_general(tribes))
})

test_that("general blockmodeling blockmat error 2 works", {
  data("tribes")
  B <- matrix(c(2, 2, 2, 2), 2, 2)
  expect_error(signed_blockmodel_general(tribes, blockmat = B))
})

test_that("general blockmodeling sign check works", {
  g <- igraph::make_full_graph(5)
  expect_error(signed_blockmodel_general(g))
})
