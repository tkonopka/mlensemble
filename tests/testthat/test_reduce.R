# test adding models into ensembles


test_that("reduce by averaging (unweighted)", {
  x <- list(a=c(2,4,6), b=c(4,6,8))
  result <- reduce_mean(x)
  expect_equal(result, c(3,5,7))
})


test_that("reduce by averaging (with weights)", {
  x <- list(a=c(2,100), b=c(10,2))
  result <- reduce_mean(x, weight=c(a=10, b=1))
  expect_lt(result[1], 5)
  expect_gt(result[2], 50)
})


test_that("reduce by averaging (matrix)", {
  x <- list(a=matrix(c(0.2, 0.8, 0.8, 0.2), byrow=TRUE, nrow=2),
            b=matrix(c(0.5, 0.5, 1.0, 0.0), byrow=TRUE, nrow=2))
  result <- reduce_mean(x)
  expect_equal(result, matrix(c(0.35, 0.65, 0.9, 0.1), byrow=TRUE, nrow=2))
})

