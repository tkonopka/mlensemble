# test using calibration on classification models

# create some simple datasets and models
if (!exists("d_linear")) {
  source("datasets_models.R")
}


# calibrating for class inbalance

test_that("calibration should adjust to signal-to-noise ratio", {
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
  expect_warning(err_pe0_t1 <- rmse(predict(me0, newdata=test_data_1), calib_x))
  err_pe1_t1 <- rmse(predict(me1, newdata=test_data_1), calib_x)
  err_pe2_t1 <- rmse(predict(me2, newdata=test_data_1), calib_x)
  expect_lt(err_pe1_t1, err_pe0_t1)
  expect_lt(err_pe1_t1, err_pe2_t1)
  # model calibrated with x1-noisy data should work well when x1 is noisy
  expect_warning(err_pe0_t2 <- rmse(predict(me0, newdata=test_data_2), calib_x))
  err_pe1_t2 <- rmse(predict(me1, newdata=test_data_2), calib_x)
  err_pe2_t2 <- rmse(predict(me2, newdata=test_data_2), calib_x)
  expect_lt(err_pe2_t2, err_pe0_t2)
  expect_lt(err_pe2_t2, err_pe1_t2)
})

