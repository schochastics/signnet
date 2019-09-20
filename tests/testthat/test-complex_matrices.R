test_that("error not graph works", {
  expect_error(as_adj_complex(g=5))
})

test_that("error directed works", {
  expect_error(as_adj_complex(igraph::graph.full(3,directed=TRUE)))
})

test_that("error directed works", {
  expect_error(as_adj_complex(igraph::graph.full(3,directed=TRUE)))
})

test_that("error no attribute works", {
  expect_error(as_adj_complex(igraph::graph.full(3,directed=FALSE)))
})

test_that("error wrong attribute works", {
  g <- igraph::graph.full(3,directed=FALSE)
  igraph::E(g)$type <- "F"
  expect_error(as_adj_complex(g,"type"))
})

test_that("complex adj works", {
  g <- igraph::graph.full(3,directed=FALSE)
  igraph::E(g)$type <- "N"
  A_true <- structure(c(0+0i, 0-1i, 0-1i, 0+1i, 0+0i, 0-1i, 0+1i, 0+1i, 0+0i
  ), .Dim = c(3L, 3L))
  A <- as_adj_complex(g,"type")
  expect_equal(A,A_true)
})
