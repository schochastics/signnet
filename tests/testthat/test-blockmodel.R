test_that("blockmodeling works", {
  data("tribes")
  clu <- signed_blockmodel(tribes,k = 3,alpha=0.5,annealing = TRUE)
  expect_equal(max(clu$membership),3)
})

test_that("blockmodeling k error works", {
  data("tribes")
  expect_error(signed_blockmodel(tribes))
})

test_that("general blockmodeling works", {
  data("tribes")
  clu <- signed_blockmodel_general(tribes,blockmat = matrix(c(1,-1,-1,
                                                              -1,1,-1,
                                                              -1,-1,1),3,3,byrow = T))
  expect_equal(max(clu$membership),3)
})

test_that("general  blockmodeling k error works", {
  data("tribes")
  expect_error(signed_blockmodel(tribes))
})
