test_that("ggblock works", {
    data("tribes")
    clu <- signed_blockmodel(tribes, k = 3, alpha = 0.5, annealing = TRUE)
    p <- ggblock(g, clu$membership, show_blocks = TRUE, show_labels = TRUE)
    expect_true(all(p$data$value == 1))
})
