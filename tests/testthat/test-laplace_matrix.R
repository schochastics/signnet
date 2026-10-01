test_that("laplacian matrix correct", {
  g <- igraph::make_full_graph(5)
  igraph::E(g)$sign <- -1
  L <- laplacian_matrix_signed(g)
  expect_equal(diag(L), rep(4, 5))
})

test_that("laplacian matrix norm correct", {
  g <- igraph::make_full_graph(5)
  igraph::E(g)$sign <- -1
  L <- laplacian_matrix_signed(g, norm = TRUE)
  L_true <- structure(
    c(
      1,
      0.25,
      0.25,
      0.25,
      0.25,
      0.25,
      1,
      0.25,
      0.25,
      0.25,
      0.25,
      0.25,
      1,
      0.25,
      0.25,
      0.25,
      0.25,
      0.25,
      1,
      0.25,
      0.25,
      0.25,
      0.25,
      0.25,
      1
    ),
    dim = c(5L, 5L)
  )
  expect_equal(L, L_true)
})

test_that("laplacian matrix error graph correct", {
  expect_error(laplacian_matrix_signed(g = 5))
})

test_that("laplacian matrix error sign correct", {
  g <- igraph::make_full_graph(5)
  expect_error(laplacian_matrix_signed(g))
})

#
# test_that("laplacian angle sign is correct", {
#   g <- igraph::make_full_graph(3)
#   igraph::E(g)$sign <- 1
#   ang <- laplacian_angle(g)
#   ang_true <- c(0.95531662, -0.61547971, -0.61547971)
# })
#
# test_that("laplacian angle sign is correct", {
#   g <- igraph::make_full_graph(3)
#   igraph::E(g)$type <- "P"
#   ang <- laplacian_angle(g,"complex",attr="type")
#   ang_true <- c(0, 0, 3.14159265)
# })

test_that("sparse signed laplacian works and matches dense", {
    g <- igraph::make_full_graph(4)
    igraph::E(g)$sign <- c(-1, 1, 1, -1, -1, 1)
    L <- laplacian_matrix_signed(g, sparse = TRUE)
    expect_s4_class(L, "Matrix")
    expect_equal(as.matrix(L), laplacian_matrix_signed(g), ignore_attr = TRUE)
    Ln <- laplacian_matrix_signed(g, norm = TRUE, sparse = TRUE)
    expect_equal(
        as.matrix(Ln),
        laplacian_matrix_signed(g, norm = TRUE),
        ignore_attr = TRUE
    )
})

test_that("normalized signed laplacian handles isolates", {
    g <- igraph::make_full_graph(3)
    igraph::E(g)$sign <- 1
    g <- igraph::add_vertices(g, 1)
    L <- laplacian_matrix_signed(g, norm = TRUE)
    expect_false(any(is.nan(L)))
})
