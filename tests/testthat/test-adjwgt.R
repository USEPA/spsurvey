context("weight adjustment")

test_that("adjust weight works scalar", {
  wgt <- runif(50)
  framesize <- 100
  wgt_new <- adjwgt(wgt, framesize = framesize)
  wgtsums <- sum(wgt_new)
  expect_equal(as.vector(wgtsums), as.vector(framesize))
})

test_that("adjust weight works without sites", {
  wgt <- runif(50)
  wgtcat <- rep(c("A", "B"), c(30, 20))
  framesize <- c(A = 15, B = 10)
  wgt_new <- adjwgt(wgt, wgtcat, framesize)
  wgtsums <- tapply(wgt_new, wgtcat, sum)
  expect_equal(names(wgtsums), names(framesize))
  expect_equal(as.vector(wgtsums), as.vector(framesize))
})

test_that("adjust weight works with sites", {
  wgt <- runif(50)
  wgtcat <- rep(c("A", "B"), c(30, 20))
  framesize <- c(A = 15, B = 10)
  sites <- rep(rep(c(TRUE, FALSE), c(9, 1)), 5)
  wgt_new <- adjwgt(wgt, wgtcat, framesize, sites)
  wgtsums <- tapply(wgt_new, wgtcat, sum)
  expect_equal(names(wgtsums), names(framesize))
  expect_equal(as.vector(wgtsums), as.vector(framesize))
})
