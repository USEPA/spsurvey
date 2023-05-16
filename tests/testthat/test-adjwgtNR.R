context("weight adjustment non-response")

set.seed(5)

test_that("adjust weight non-response works no extra Eval", {
  wgt <- runif(30)
  MARClass <- rep(c("A", "B"), rep(15, 2))
  EvalStatus <- rep(c("Target_Sampled", "Target_Not_Sampled"), 15)
  TNRClass <- "Target_Not_Sampled"
  TRClass <- "Target_Sampled"
  wgt_new <- adjwgtNR(wgt, MARClass, EvalStatus, TNRClass, TRClass)
  wgtsums <- sum(wgt_new[EvalStatus == "Target_Sampled"])
  wgtzeros <- sum(wgt_new[EvalStatus == "Target_Not_Sampled"])
  expect_equal(sum(wgt), wgtsums)
  expect_equal(0, wgtzeros)
  
  # error returned
  EvalStatus[EvalStatus == "Target_Sampled"] <- "Target_Not_Sampled"
  expect_error(adjwgtNR(wgt, MARClass, EvalStatus, TNRClass, TRClass))
})

test_that("adjust weight non-response works extra Eval", {
  
  wgt <- runif(30)
  MARClass <- rep(c("A", "B"), rep(15, 2))
  EvalStatus <- rep(c("Not_Target", "Target_Sampled", "Target_Not_Sampled"), 10)
  TNRClass <- "Target_Not_Sampled"
  TRClass <- "Target_Sampled"
  wgt_new <- adjwgtNR(wgt, MARClass, EvalStatus, TNRClass, TRClass)
  wgtsums <- sum(wgt_new[EvalStatus == "Target_Sampled"])
  wgtzeros <- sum(wgt_new[EvalStatus == "Target_Not_Sampled"])
  expect_equal(sum(wgt[EvalStatus %in% c("Target_Not_Sampled", "Target_Sampled")]), wgtsums)
  expect_equal(0, wgtzeros)
  wgtzeros_extra <- sum(wgt_new[!EvalStatus %in% c("Target_Not_Sampled", "Target_Sampled")])
  expect_equal(0, wgtzeros)
  
  # error returned
  EvalStatus[EvalStatus == "Target_Sampled"] <- "Not_Target"
  expect_error(adjwgtNR(wgt, MARClass, EvalStatus, TNRClass, TRClass))
})

