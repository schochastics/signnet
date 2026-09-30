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

# independent reference implementation of the blockmodel criterion
crit_ref <- function(g, membership, blockmat, alpha) {
  A <- as_adj_signed(g)
  idx <- which(A != 0, arr.ind = TRUE)
  a <- A[idx]
  expected <- blockmat[cbind(membership[idx[, 1]], membership[idx[, 2]])]
  sum(ifelse(a < 0 & expected == 1, alpha, 0) +
    ifelse(a > 0 & expected == -1, 1 - alpha, 0))
}

example_general <- function() {
  g1 <- g2 <- g3 <- igraph::make_full_graph(5)
  igraph::V(g1)$name <- as.character(1:5)
  igraph::V(g2)$name <- as.character(6:10)
  igraph::V(g3)$name <- as.character(11:15)
  g <- Reduce(igraph::`%u%`, list(g1, g2, g3))
  igraph::E(g)$sign <- 1
  igraph::E(g)$sign[1:10] <- -1
  g <- igraph::add_edges(g, c(rbind(1:5, 6:10)), attr = list(sign = -1))
  g <- igraph::add_edges(g, c(rbind(1:5, 11:15)), attr = list(sign = -1))
  igraph::add_edges(g, c(rbind(11:15, 6:10)), attr = list(sign = 1))
}

test_that("general blockmodel criterion matches membership", {
  g <- example_general()
  blockmat <- matrix(c(1, -1, -1, -1, 1, 1, -1, 1, -1), 3, 3, byrow = TRUE)
  for (alpha in c(0.2, 0.5, 0.8)) {
    set.seed(1)
    clu <- signed_blockmodel_general(g, blockmat, alpha)
    expect_gte(clu$criterion, 0)
    expect_equal(clu$criterion, crit_ref(g, clu$membership, blockmat, alpha))
  }
  set.seed(1)
  expect_equal(signed_blockmodel_general(g, blockmat, 0.5)$criterion, 0)
})

test_that("blockmodel criterion matches membership", {
  data("tribes")
  for (annealing in c(TRUE, FALSE)) {
    set.seed(1)
    clu <- signed_blockmodel(tribes, k = 3, alpha = 0.3, annealing = annealing)
    blockmat <- 2 * diag(3) - 1
    expect_equal(clu$criterion, crit_ref(tribes, clu$membership, blockmat, 0.3))
  }
  set.seed(1)
  expect_equal(signed_blockmodel(tribes, k = 3, annealing = TRUE)$criterion, 2)
})

test_that("greedy blockmodel improves large networks", {
  set.seed(1)
  g <- sample_islands_signed(2, 1100, 0.005, 20)
  clu <- signed_blockmodel(g, k = 2)
  # a random partition violates about ecount(g) entries
  expect_lt(clu$criterion, igraph::ecount(g) / 2)
})

test_that("blockmodel argument checks work", {
  data("tribes")
  expect_error(signed_blockmodel(tribes, k = 0))
  expect_error(signed_blockmodel(tribes, k = 1.5))
  expect_error(signed_blockmodel(tribes, k = 100))
  expect_error(signed_blockmodel(tribes, k = 2, alpha = 2))
  expect_error(signed_blockmodel_general(tribes, matrix(1, 2, 3)))
  expect_error(
    signed_blockmodel_general(tribes, matrix(c(1, 1, -1, 1), 2, 2)),
    "symmetric"
  )
})
