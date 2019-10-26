context("regr.extratrees")

test_that("autotest", {
  learner = LearnerRegrExtraTrees$new()
  expect_learner(learner)
})
