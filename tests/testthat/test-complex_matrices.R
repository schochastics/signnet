test_that("incidence: error not graph works", {
    expect_error(as_incidence_signed(g = 5))
})

test_that("incidence: error no sign works", {
    expect_error(as_incidence_signed(igraph::graph.full(3, directed = FALSE)))
})

test_that("incidence: error no type works", {
    g <- igraph::graph.full(3, directed = FALSE)
    igraph::E(g)$sign <- 1
    expect_error(as_incidence_signed(g))
})

test_that("signed incidence works", {
    A_true <- matrix(c(
        1, 1, 1, -1, -1, -1, -1,
        1, 1, 1, -1, -1, -1, -1,
        1, 1, 1, -1, -1, -1, -1,
        -1, -1, -1, 1, 1, 1, 1,
        -1, -1, -1, 1, 1, 1, 1
    ), 5, 7, byrow = T)
    rownames(A_true) <- letters[1:5]
    colnames(A_true) <- 1:7
    g <- igraph::graph_from_incidence_matrix(A_true, weighted = "sign")
    A <- as_incidence_signed(g)
    expect_equal(A, A_true)
})


test_that("complex: error not graph works", {
    expect_error(as_adj_complex(g = 5))
})

test_that("complex: error directed works", {
    expect_error(as_adj_complex(igraph::graph.full(3, directed = TRUE)))
})

test_that("complex: error no attribute works", {
    expect_error(as_adj_complex(igraph::graph.full(3, directed = FALSE)))
})

test_that("complex: error wrong attribute works", {
    g <- igraph::graph.full(3, directed = FALSE)
    igraph::E(g)$type <- "F"
    expect_error(as_adj_complex(g, "type"))
})

test_that("complex: complex adj works", {
    g <- igraph::graph.full(3, directed = FALSE)
    igraph::E(g)$type <- "N"
    A_true <- structure(c(0 + 0i, 0 - 1i, 0 - 1i, 0 + 1i, 0 + 0i, 0 - 1i, 0 + 1i, 0 + 1i, 0 + 0i), .Dim = c(3L, 3L))
    A <- as_adj_complex(g, "type")
    expect_equal(A, A_true)
})

test_that("signed: error not graph works", {
    expect_error(as_adj_signed(g = 5))
})

test_that("signed: error attribute works", {
    expect_error(as_adj_complex(igraph::graph.full(3)))
})

test_that("complex laplace: error not graph works", {
    expect_error(laplacian_matrix_complex(g = 5))
})

test_that("complex laplace: error directed works", {
    expect_error(laplacian_matrix_complex(igraph::graph.full(3, directed = TRUE)))
})

test_that("complex laplacian: complex laplacian works", {
    g <- igraph::graph.full(3, directed = FALSE)
    igraph::E(g)$type <- "P"

    L <- laplacian_matrix_complex(g, "type")
    L_true <- structure(c(2 + 0i, -1 + 0i, -1 + 0i, -1 + 0i, 2 + 0i, -1 + 0i, -1 + 0i, -1 + 0i, 2 + 0i), .Dim = c(3L, 3L))
    expect_equal(L, L_true)
})

test_that("complex laplacian: complex laplacian norm works", {
    g <- igraph::graph.full(3, directed = FALSE)
    igraph::E(g)$type <- "P"

    L <- laplacian_matrix_complex(g, "type", norm = TRUE)
    L_true <- structure(c(1 + 0i, -0.5 + 0i, -0.5 + 0i, -0.5 + 0i, 1 + 0i, -0.5 + 0i, -0.5 + 0i, -0.5 + 0i, 1 + 0i), .Dim = c(3L, 3L))
    expect_equal(L, L_true)
})

test_that("complex incidence: incidence works", {
    g <- igraph::graph.full(3, directed = FALSE)
    igraph::E(g)$type <- c("P", "N", "A")

    S <- round(as_incidence_complex(g, "type"), 4)
    S_true <- structure(c(
        1 + 0i, -1 + 0i, 0 + 0i, 0.7071 + 0.7071i, 0 + 0i, -0.7071 + 0.7071i,
        0 + 0i, 0.7769 + 0.3218i, -0.7769 + 0.3218i
    ), .Dim = c(3L, 3L))
    expect_equal(S, S_true)
})

test_that("as_complex_edges works", {
    g <- igraph::graph.full(3, directed = FALSE)
    igraph::E(g)$sign <- -1
    g1 <- as_complex_edges(g)
    expect_equal(igraph::E(g1)$type, c("N", "N", "N"))
})

test_that("as_complex_edges error not graph works", {
    expect_error(as_complex_edges(g = 5))
})

test_that("as_complex_edges error no sign works", {
    g <- igraph::graph.full(3, directed = FALSE)
    # igraph::E(g)$sign <- -1
    expect_error(as_complex_edges(g))
})

test_that("as_complex_edges error wrong sign works", {
    g <- igraph::graph.full(3, directed = FALSE)
    igraph::E(g)$sign <- 3
    expect_error(as_complex_edges(g))
})

test_that("as_unsigned_2mode works for FALSE", {
    df <- data.frame(from = c(1, 1, 1, 2, 2), to = c("a", "b", "c", "a", "b"), sign = c(1, 1, -1, 1, -1))
    vert <- data.frame(name = c(1, 2, "a", "b", "c"), type = c(TRUE, TRUE, FALSE, FALSE, FALSE))
    g <- igraph::graph_from_data_frame(df, directed = FALSE, vertices = vert)

    g1 <- as_unsigned_2mode(g, primary = FALSE)
    el_true <- structure(
        list(
            from = c("a-pos", "b-pos", "c-neg", "a-pos", "b-neg"),
            to = c("1", "1", "1", "2", "2")
        ),
        class = "data.frame", row.names = c(NA, 5L)
    )
    expect_equal(igraph::as_data_frame(g1, "edges"), el_true)
})

test_that("as_unsigned_2mode works for TRUE", {
    df <- data.frame(from = c(1, 1, 1, 2, 2), to = c("a", "b", "c", "a", "b"), sign = c(1, 1, -1, 1, -1))
    vert <- data.frame(name = c(1, 2, "a", "b", "c"), type = c(TRUE, TRUE, FALSE, FALSE, FALSE))
    g <- igraph::graph_from_data_frame(df, directed = FALSE, vertices = vert)

    g1 <- as_unsigned_2mode(g, primary = TRUE)
    el_true <- structure(
        list(
            from = c("1-pos", "1-pos", "1-neg", "2-pos", "2-neg"),
            to = c("a", "b", "c", "a", "b")
        ),
        class = "data.frame",
        row.names = c(NA, 5L)
    )
    expect_equal(igraph::as_data_frame(g1, "edges"), el_true)
})

test_that("as_unsigned_2mode 2mode error works", {
    df <- data.frame(from = c(1, 1, 1, 2, 2), to = c("a", "b", "c", "a", "b"), sign = c(1, 1, -1, 1, -1))
    vert <- data.frame(name = c(1, 2, "a", "b", "c"))
    g <- igraph::graph_from_data_frame(df, directed = FALSE, vertices = vert)
    expect_error(as_unsigned_2mode(g, primary = TRUE))
})

test_that("as_signed_proj works", {
    df <- data.frame(from = c(1, 1, 1, 2, 2), to = c("a", "b", "c", "a", "b"), sign = c(1, 1, -1, 1, -1))
    vert <- data.frame(name = c(1, 2, "a", "b", "c"), type = c(T, T, F, F, F))
    g <- igraph::graph_from_data_frame(df, directed = FALSE, vertices = vert)

    g1 <- as_unsigned_2mode(g, primary = TRUE)
    p <- igraph::bipartite_projection(g1, which = "true")
    p1 <- as_signed_proj(p)
    el <- igraph::as_data_frame(p1, "edges")
    el_true <- structure(list(from = "1", to = "2", type = "A"), class = "data.frame", row.names = 1L)
    expect_equal(el, el_true)
})

test_that("complex walks works", {
    g <- igraph::graph.full(3, directed = FALSE)
    igraph::E(g)$type <- c("P", "P", "N")
    W <- complex_walks(g, "type", 3)
    W_true <- structure(c(0 + 2i, 3 + 0i, 3 + 0i, 3 + 0i, 0 + 2i, 0 + 3i, 3 + 0i, 0 + 3i, 0 + 2i), .Dim = c(3L, 3L))
    expect_equal(W, W_true)
})
