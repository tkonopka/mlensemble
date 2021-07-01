# test adding models into ensembles

# create some simple datasets and models
if (!exists("d_linear")) {
  source("datasets_models.R")
}


test_that("create simple ml_model objects", {
  result <- ml_model(m_lm_1)
  expect_true(is(result, "ml_model"))
  expect_equal(result$model_name, "m_lm_1")
  expect_equal(result$feature_names, c("x1", "x2"))
})


test_that("create simple ml_ensemble objects", {
  result <- ml_ensemble()
  expect_true(is(result, "ml_model"))
  expect_true(is(result, "ml_ensemble"))
  expect_equal(result$model_name, "ml-ensemble")
})


test_that("composition suite + test gives a larger suite", {
  result <- ml_model(m_lm_1) + ml_model(m_lm_2)
  expect_true(is(result, "ml_model"))
  expect_true(is(result, "ml_ensemble"))
  expect_equal(length(result$models), 2)
})

