# test helper methods used during calibration


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

