test_that("blockmodeling works", {
  data("tribes")
  clu <- signed_blockmodel(tribes,k = 3,alpha=0.5,annealing = TRUE)
  expect_equal(max(clu$membership),3)
})

test_that("blockmodeling k error works", {
  data("tribes")
  expect_error(signed_blockmodel(tribes))
})
