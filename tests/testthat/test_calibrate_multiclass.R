# test calibration of classification models

# create some simple datasets and models
if (!exists("d_linear")) {
  source("datasets_models.R")
  source("helpers.R")
}


test_that("calibration should work even with a single model", {
  me0 <- ml_ensemble() + ml_model(m_mc_pop0)
  # calibration and test data
  calib_data <- data.table(cbind(x=rnorm(30), y=rnorm(30)))
  calib_data$label <- rep(c(0, 1), nrow(calib_data)/2)
  expect_silent(p0 <- predict(me0, data=as.matrix(calib_data)))
  me1 <- suppressWarnings(calibrate(me0,
                                    data=as.matrix(calib_data[, c("x", "y")]),
                                    label=calib_data$label))
  expect_silent(p1 <- predict(me1, data=as.matrix(calib_data)))
  # there should be two calibration models (two labels)
  expect_equal(length(me1$calibration), 2)
  # the outputs without and with calibration should be slightly different
  expect_false(identical(p0, p1))
})


test_that("calibration should mitigate class inbalance", {
  # m_mc_pop0 and m_mc_pop1 are trained with different proportions of
  # items in class 0 and 1
  m0 <- ml_model(m_mc_pop0)
  m1 <- ml_model(m_mc_pop1)
  me0 <- ml_model(m_mc_pop0) + ml_model(m_mc_pop1)
  # calibration and test data have equal proportions of two classes
  calib_data <- data.table(cbind(x=rnorm(20, 0, 2),
                                 y=rnorm(20, 0, 2)))
  calib_data$label <- rep(c(0,1), nrow(calib_data)/2)
  test_data <- cbind(x=rnorm(40, 0, 2), y=rnorm(40, 0, 2))
  me1 <- calibrate(me0,
                   data=as.matrix(calib_data[, c("x", "y")]),
                   label=calib_data$label)
  # compute predictions
  p0 <- apply(predict(m0, data=test_data), 1, which.max)-1
  p1 <- apply(-1+predict(m1, data=test_data), 1, which.max)-1
  expect_warning(pe0 <- apply(predict(me0, data=test_data),
                              1, which.max)-1)
  pe1 <- apply(predict(me1, data=test_data), 1, which.max)-1
  # compute the fraction of points that fall into the dominant class
  dom_frac <- function(z) {
    max(table(z)) / length(z)
  }
  # ensemble should even out inbalance
  expect_lte(dom_frac(pe0), dom_frac(p1))
  expect_lte(dom_frac(pe0), dom_frac(p0))
  expect_lte(dom_frac(pe1), dom_frac(p1))
  expect_lte(dom_frac(pe1), dom_frac(p0))
})


test_that("calibration with models predicting different labels", {
  # train two models for labels (Q1, Q2, Q3) and one model for (Q1, Q2, Q4)
  d1 <- d_mc_1[d_mc_1$label!=3, ]
  d2 <- d_mc_2[d_mc_2$label!=2, ]
  d2[d2$label==3, "label"] <- 2
  xgb1 <- xgboost(as.matrix(d1[, c("x", "y")]),
                  label=d1$label, nrounds=2, num_class=3, verbose=0,
                  objective="multi:softprob")
  xgb2 <- xgboost(as.matrix(d2[, c("x", "y")]),
                  label=d2$label, nrounds=2, num_class=3, verbose=0,
                  objective="multi:softprob")
  m1 <- ml_model(xgb1, label_names=c("Q1", "Q2", "Q3"))
  m2 <- ml_model(xgb2, label_names=c("Q1", "Q2", "Q4"))
  me <- m1 + m2
  test_data <- as.matrix(testdata_mc[, c("x", "y")])
  # predictions (warning because the ensemble is not calibrated)
  p1 <- predict(m1, data=test_data)
  p2 <- predict(m2, data=test_data)
  expect_warning(pe <- predict(me, data=test_data))
  # p1 only predicts Q1, Q2, Q3
  expect_equal(sum(call_softmax(p1)=="Q4"), 0)
  # p2 only predicts (q1, Q3, Q4)
  expect_equal(sum(call_softmax(p2)=="Q3"), 0)
  # pe predicts all labels
  expect_gt(sum(call_softmax(pe)=="Q3"), 0)
  expect_gt(sum(call_softmax(pe)=="Q4"), 0)
})


test_that("calibration can eliminate labels", {
  me <- ml_model(m_mc_1) + ml_model(m_mc_2)
  n <- nrow(testdata_mc)
  # calibration data using only points in quadrants 0, 1, 2
  i_even <- seq(1, n) %% 2 ==0
  calib_data_4 <- as.matrix(testdata_mc[i_even, c("x", "y")])
  calib_labels_4 <- testdata_mc[i_even, ]$label
  calib_data_4[, "x"] <- calib_data_4[, "x"] + rnorm(sum(i_even), 0, 0.8)
  calib_data_4[, "y"] <- calib_data_4[, "y"] + rnorm(sum(i_even), 0, 0.8)
  calib_data_3 <- calib_data_4[calib_labels_4<3, ]
  calib_labels_3 <- calib_labels_4[calib_labels_4<3]
  # test data using all quadrants
  test_data <- as.matrix(testdata_mc[!i_even, c("x", "y")])
  test_labels <- testdata_mc[!i_even, ]$label
  mec3 <- suppressWarnings(calibrate(me, calib_data_3, calib_labels_3))
  mec4 <- suppressWarnings(calibrate(me, calib_data_4, calib_labels_4))
  oute3 <- predict(mec3, test_data)
  oute4 <- predict(mec4, test_data)
  # four-quadrant calibration will predict label_3 sometimes
  expect_gt(max(oute4[, "label_3"]), 0.5)
  # three quadrant calibration will not predict label_3
  expect_lt(max(oute3[, "label_3"]), 0.1)
})

