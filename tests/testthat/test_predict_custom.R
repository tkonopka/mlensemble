# test using custom function-based models

# create some simple datasets and models
if (!exists("d_linear")) {
  source("datasets_models.R")
  source("helpers.R")
}


test_that("predict single using an ml_model based on a custom function", {
  my_pred <- predict_negative_or_positive
  m <- ml_model(my_pred, feature_names="x",
                label_names=c("negative", "positive"))
  test_data <- cbind(x=2, y=2)
  result <- predict(m, newdata=test_data)
  # result should be a matrix with class probabilities
  expect_equal(dim(result), c(1, 2))
  # the first column in test_data is positive, so the the prediction should
  # be that the data points is of "positive" class
  expect_equal(as.numeric(result[, "positive"]), 1)
  expect_equal(as.numeric(result[, "negative"]), 0)
})


test_that("predict many using an ml_model based on a custom function", {
  my_pred <- predict_negative_or_positive
  m <- ml_model(my_pred, feature_names=c("x", "y"),
                label_names=c("negative", "positive"))
  test_data <- cbind(x=c(2, 0, -1, -2), y=2)
  result <- predict(m, newdata=test_data)
  # result should be a matrix with class probabilities
  expect_equal(dim(result), c(4, 2))
  # the first column in test_data is positive, so the the prediction should
  # be that the data points is of "positive" class
  expect_equal(as.numeric(result[, "positive"]), c(1, 0.5, 0, 0))
  expect_equal(as.numeric(result[, "negative"]), c(0, 0.5, 1, 1))
})

