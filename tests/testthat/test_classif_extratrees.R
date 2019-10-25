context("classif.extratrees")

test_that("autotest", {
  learner = LearnerClassifExtraTrees$new()
  expect_learner(learner)
})
