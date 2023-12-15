test_that("ggblock works", {
    data("tribes")
    clu <- signed_blockmodel(tribes, k = 3, alpha = 0.5, annealing = TRUE)
    p <- ggblock(tribes, clu$membership, show_blocks = TRUE, show_labels = TRUE)
    expect_true(all(p$data$value %in% c(-1, 1)))
    expect_equal(length(p$layers), 3)
})
