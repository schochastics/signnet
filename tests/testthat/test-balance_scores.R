test_that("triangle balance index works", {
    g <- igraph::make_full_graph(5)
    igraph::E(g)$sign <- 1
    expect_equal(balance_score(g, method = "triangles"), 1)
})

test_that("walk balance index works", {
    g <- igraph::make_full_graph(5)
    igraph::E(g)$sign <- 1
    expect_equal(balance_score(g, method = "walk"), 1)
})

test_that("frustration balance index works", {
    g <- igraph::make_full_graph(5)
    igraph::E(g)$sign <- 1
    expect_equal(balance_score(g, method = "frustration"), 1)
})


test_that("directed check works", {
    g <- igraph::make_full_graph(5, directed = TRUE)
    igraph::E(g)$sign <- 1
    expect_error(balance_score(g))
})

test_that("sign check works", {
    g <- igraph::make_full_graph(5, directed = FALSE)
    expect_error(balance_score(g))
})

test_that("wrong sign values check works", {
    g <- igraph::make_full_graph(5, directed = FALSE)
    igraph::E(g)$sign <- 2
    expect_error(balance_score(g))
})

test_that("frustration exact error handling", {
    g <- igraph::make_full_graph(5, directed = FALSE)
    igraph::E(g)$sign <- 2
    expect_error(frustration_exact(g))
    g <- igraph::make_full_graph(5, directed = TRUE)
    igraph::E(g)$sign <- 1
    expect_error(frustration_exact(g))
})

test_that("walk balance does not overflow on dense graphs", {
    g <- igraph::make_full_graph(800)
    igraph::E(g)$sign <- 1
    expect_equal(balance_score(g, method = "walk"), 1)
    igraph::E(g)$sign[1:1000] <- -1
    b <- balance_score(g, method = "walk")
    expect_true(is.finite(b) && b > 0 && b <= 1)
})

test_that("triangle balance is NA without triangles", {
    g <- igraph::make_ring(4)
    igraph::E(g)$sign <- 1
    expect_warning(res <- balance_score(g, method = "triangles"))
    expect_true(is.na(res))
})
