test_that("ggblock works", {
    data("tribes")
    clu <- signed_blockmodel(tribes, k = 3, alpha = 0.5, annealing = TRUE)
    p <- ggblock(tribes, clu$membership, show_blocks = TRUE, show_labels = TRUE)
    expect_true(all(p$data$value %in% c(-1, 1)))
    expect_equal(length(p$layers), 3)
})

test_that("ggblock colors positive-only networks correctly", {
    g <- igraph::make_full_graph(4)
    igraph::E(g)$sign <- 1
    b <- ggplot2::ggplot_build(ggblock(g))
    expect_equal(unique(b$data[[1]]$fill), "steelblue")
})

test_that("ggsigned complex uses attr", {
    skip_if_not_installed("ggraph")
    g <- igraph::make_full_graph(4)
    igraph::E(g)$foo <- c("P", "N", "A", "A", "P", "N")
    p <- ggsigned(g, type = "complex", attr = "foo")
    expect_no_warning(b <- ggplot2::ggplot_build(p))
    expect_equal(length(unique(b$data[[1]]$edge_colour)), 3)
})
