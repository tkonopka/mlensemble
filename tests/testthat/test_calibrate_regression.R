# test using calibration on regression models

# create some simple datasets and models
if (!exists("d_linear")) {
  source("datasets_models.R")
}


test_that("calibration should adjust to signal-to-noise ratio", {
  # data with simple linear relation y = x
  m1 <- ml_model(m_lm_x1)
  m2 <- ml_model(m_lm_x2)
  me0 <- ml_model(m_lm_x1) + ml_model(m_lm_x2)
  n <- 20
  calib_x <- seq(0, 10, length=n)
  # calib_1 has low signal-to-noise ratio in x1, calib_2 is better in x2
  calib_data_1 <- data.frame(x1=calib_x + rnorm(n, 0, 0.5),
                             x2=calib_x + rnorm(n, 0, 4),
                             y=calib_x + rnorm(n))
  calib_data_2 <- data.frame(x1=calib_x + rnorm(n, 0, 4),
                             x2=calib_x + rnorm(n, 0, 0.5),
                             y=calib_x + rnorm(n))
  test_data_1 <- data.frame(x1=calib_x + rnorm(n, 0, 0.1),
                            x2=calib_x + rnorm(n, 0, 5))
  test_data_2 <- data.frame(x1=calib_x + rnorm(n, 0, 5),
                            x2=calib_x + rnorm(n, 0, 0.1))
  # calibration of models
  me1 <- calibrate(me0, data=calib_data_1, label=calib_data_1$y)
  me2 <- calibrate(me0, data=calib_data_2, label=calib_data_2$y)
  # compute predictions and errors
  # model calibrated with x2-noisy data should work well when x2 is noisy
  expect_warning(err_pe0_t1 <- rmse(predict(me0, data=test_data_1), calib_x))
  err_pe1_t1 <- rmse(predict(me1, data=test_data_1), calib_x)
  err_pe2_t1 <- rmse(predict(me2, data=test_data_1), calib_x)
  expect_lt(err_pe1_t1, err_pe0_t1)
  expect_lt(err_pe1_t1, err_pe2_t1)
  # model calibrated with x1-noisy data should work well when x1 is noisy
  expect_warning(err_pe0_t2 <- rmse(predict(me0, data=test_data_2), calib_x))
  err_pe1_t2 <- rmse(predict(me1, data=test_data_2), calib_x)
  err_pe2_t2 <- rmse(predict(me2, data=test_data_2), calib_x)
  expect_lt(err_pe2_t2, err_pe0_t2)
  expect_lt(err_pe2_t2, err_pe1_t2)
})


test_that("calibration can use weights", {
  # data with simple linear relation y = x
  # ensemble with models trained on two measurements of x (x1 and x2)
  me <- ml_model(m_lm_x1) + ml_model(m_lm_x2)
  n <- 20
  calib_x <- seq(0, 10, length=n)
  calib_w <- rep(1, n)
  # calib_data has informative information, but a few outliers
  calib_data <- data.frame(x1=calib_x + rnorm(n, 0, 1),
                           x2=calib_x + rnorm(n, 0, 1),
                           y=calib_x + rnorm(n))
  calib_data$y[4] <- 20
  calib_data$x1[5] <- (-2)
  calib_data$x2[9] <- (-5)
  calib_w[c(4,5,9)] <- 0
  test_data <- data.frame(x1=calib_x + rnorm(n, 0, 1),
                          x2=calib_x + rnorm(n, 0, 1))
  # calibration of models, plain and with weights
  me0 <- calibrate(me, data=calib_data, label=calib_data$y)
  mew <- calibrate(me, data=calib_data, label=calib_data$y, weight=calib_w)
  p0 <- predict(me0, test_data)
  pw <- predict(mew, test_data)
  err_me0 <- rmse(predict(me0, test_data), calib_x)
  err_mew <- rmse(predict(mew, test_data), calib_x)
  # calibration that avoids bad data points should give a smaller error
  expect_lt(err_mew, err_me0)
})

