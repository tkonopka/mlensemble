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


test_that("composition of two models gives an ensemble", {
  result <- ml_model(m_lm_1) + ml_model(m_lm_2)
  expect_true(is(result, "ml_model"))
  expect_true(is(result, "ml_ensemble"))
  expect_equal(length(result$models), 2)
})


test_that("composition of an ensemble and a model gives an ensemble", {
  result <- ml_ensemble("ens") + ml_model(m_lm_1)
  expect_true(is(result, "ml_ensemble"))
  expect_equal(result$model_name, "ens")
  expect_equal(length(result$models), 1)
})


test_that("composition of a model+ensemble gives an ensemble (tricky)", {
  result <-  ml_model(m_lm_1) + ml_ensemble("ens")
  expect_true(is(result, "ml_ensemble"))
  # this is a potential gotcha - the new ensemble has m_lm_1 and ml-ensemble
  # i.e. the whole ml-ensemble object is treated as a single ml_model
  expect_equal(result$model_name, "ml-ensemble")
  expect_equal(length(result$models), 2)
})


test_that("composition of two ensembles gives a larger ensemble", {
  me1 <- ml_ensemble("ens1") + ml_model(m_lm_1)
  me2 <- ml_ensemble("ens2") + ml_model(m_lm_2)
  result12 <- me1 + me2
  result21 <- me2 + me1
  expect_true(is(result12, "ml_model"))
  expect_true(is(result12, "ml_ensemble"))
  # names for the ensemble take from the left-most entry
  expect_equal(result12$model_name, "ens1")
  expect_equal(result21$model_name, "ens2")
  expect_equal(length(result12$models), 2)
})


test_that("create ml_model using a prediction function", {
  # constructor with a custom function must provide feature names
  expect_error(ml_model(predict_negative_or_positive), "feature names")
  result <- ml_model(predict_negative_or_positive, feature_names="a")
  expect_true(is(result, "ml_model"))
  expect_equal(result$model_name, "predict_negative_or_positive")
  expect_equal(result$feature_names, "a")
})
