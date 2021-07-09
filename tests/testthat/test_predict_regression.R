# test using regression models to generate predictions

# create some simple datasets and models
if (!exists("d_linear")) {
  source("datasets_models.R")
  source("helpers.R")
}


test_that("predict a single value with ml_model", {
  m <- ml_model(m_lm_1)
  testdata <- data.frame(x1=3.5, x2=4.5)
  result <- predict(m, data=testdata)
  # result should be a single value, roughly the average of x1 and x2
  expect_equal(length(result), 1)
  expect_equal(as.numeric(result), 4, tolerance=2)
})


test_that("predict a single value with ml_ensemble", {
  m <- ml_model(m_lm_1) + ml_model(m_lm_2)
  testdata <- data.frame(x1=3.5, x2=4.5)
  # warning because ensemble is not calibrated
  expect_warning(result <- predict(m, data=testdata))
  # result should be a single value, roughly the mean of x1 and x2
  expect_equal(length(result), 1)
  expect_equal(as.numeric(result), 4, tolerance=2)
})


test_that("predict many values with ml_model", {
  m <- ml_model(m_lm_1)
  testdata <- data.frame(x1=6:15, x2=6:15)
  result <- predict(m, data=testdata)
  expect_equal(length(result), nrow(testdata))
})


test_that("predict values with ml_model, too many features", {
  m <- ml_model(m_lm_1)
  testdata <- data.frame(x1=1:4, x2=1:4, x3=1:4)
  result_3 <- predict(m, data=testdata)
  result_2 <- predict(m, data=testdata[, c("x1", "x2")])
  # results are based on two-variable data and three-variable data, but the
  # model m_lm_1 uses only (x1, x2), so extra variable should have no impact
  expect_equal(result_2, result_3)
  expect_equal(length(result_3), nrow(testdata))
})


test_that("predict values with ml_model, missing features", {
  m <- ml_model(m_lm_1)
  testdata <- data.frame(x1=1:4)
  # the model expect features x1 and x2, but one of the features is missing
  # the model should still produce some output
  result <- predict(m, data=testdata)
  expect_equal(length(result), nrow(testdata))
})


test_that("predict many values with ml_model, too many variables", {
  m <- ml_model(m_lm_1)
  testdata <- data.frame(x1=1:4, x2=1:4, x3=1:4)
  result <- predict(m, data=testdata)
  expect_equal(length(result), nrow(testdata))
})


test_that("predict many values with ml_ensemble", {
  m <- ml_model(m_lm_1) + ml_model(m_lm_2)
  testdata <- data.frame(x1=6:15, x2=6:15)
  # warning because ensemble is not calibrated
  expect_warning(result <- predict(m, data=testdata))
  expect_equal(length(result), nrow(testdata))
})


test_that("predict with an empty ml_ensemble should give an error", {
  me <- ml_ensemble("my_ensemble")
  testdata <- data.frame(x1=6:15, x2=6:15)
  expect_error(predict(me, data=testdata))
})


test_that("predictions with ensemble should be more accurate that ml_model", {
  m1 <- ml_model(m_lm_1)
  m2 <- ml_model(m_lm_2)
  me <- ml_model(m_lm_1) + ml_model(m_lm_2) + ml_model(m_lm_3)
  testdata <- data.frame(x1=3:18, x2=3:18)
  expected <- apply(testdata, 1, mean)
  e1 <- rmse(predict(m1, data=testdata), expected)
  e2 <- rmse(predict(m2, data=testdata), expected)
  expect_warning(ee <- rmse(predict(me, data=testdata), expected))
  # error for ensemble should be smaller than the worst individual model
  expect_lt(ee, max(e1, e2))
})


test_that("predict with models that usedifferent features", {
  m1 <- ml_model(m_lm_x1)
  m2 <- ml_model(m_lm_x2)
  me <- ml_model(m_lm_x1) + ml_model(m_lm_x2)
  testdata <- data.frame(x1=1:20, x2=1:20)
  expected <- 1:20
  e1 <- rmse(predict(m1, data=testdata), expected)
  e2 <- rmse(predict(m2, data=testdata), expected)
  expect_warning(ee <- rmse(predict(me, data=testdata), expected))
  # error for ensembl should be smaller than the worst individual model
  expect_lt(ee, max(e1, e2))
})

