# tests for helper functions used during calibration


test_that("standardize raw predictions", {
  x <- list(a=c(2,4,6), b=c(4,6,8))
  result <- standardize_raw(x)
  expect_equal(length(result), 2)
  expect_equal(dim(result[[1]]), c(3, 1))
  expect_equal(dim(result[[2]]), c(3, 1))
})


test_that("standardize labels into a column vector", {
  x <- c(2, 1, 0)
  result <- label_matrix(x, FALSE)
  expect_is(result, "matrix")
  expect_equal(ncol(result), 1)
  expect_equal(colnames(result), "output")
  expect_equal(result[,1], x)
})


test_that("standardize labels into a class matrix", {
  x <- c(2,1,0)
  result <- label_matrix(x, TRUE)
  expect_equal(dim(result), c(3, 3))
  expect_equal(result[, "label_0"], c(0, 0, 1))
  expect_equal(result[, "label_1"], c(0, 1, 0))
  expect_equal(result[, "label_2"], c(1, 0, 0))
})


test_that("example in calibrate", {
  y_eq_x <- data.frame(x=1:10, y=1:10)
  ensemble <- ml_ensemble() + ml_model(glm(y~x, data=y_eq_x))
  # simple calibration on new data with a shift, y=x+2
  y_eq_x_2 <- data.frame(x=1:6, y=1:6 + 2)
  ensemble_calibrated <- calibrate(ensemble, data=y_eq_x_2, label=y_eq_x_2$y)
  p_calibrated <- predict(ensemble_calibrated, y_eq_x_2)
  expect_equal(as.numeric(p_calibrated), 1:6 + 2)
  # calibration on new data with a shift, y=x+1, and an outlier
  y_eq_x_outliers <- data.frame(x=1:6, y=c(1:5, 99) + 1)
  ensemble_calibrated_weights <-
    calibrate(ensemble, y_eq_x_outliers, y_eq_x_outliers$y, weight=c(1:5, 0))
  # if weighting works, the outlier value for y will be ignored, and the
  # predictions should set y=x+1
  p_weights <- predict(ensemble_calibrated_weights, y_eq_x_outliers)
  expect_equal(as.numeric(p_weights), 1:6 + 1)
})

