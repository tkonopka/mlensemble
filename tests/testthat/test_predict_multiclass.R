# test using classification models to generate class labels

# create some simple datasets and models
if (!exists("d_linear")) {
  source("datasets_models.R")
  source("helpers.R")
}


test_that("predict a single class with ml_model", {
  m <- ml_model(m_mc_1)
  test_data <- cbind(x=2, y=2)
  result <- predict(m, data=test_data)
  # result should be a matrix with class probabilities
  expect_equal(dim(result), c(1, 4))
  # the positive quadrant is the first class (by default label_0)
  expect_equal(as.integer(which.max(result[1,])), 1)
  expect_equal(call_softmax(result), "label_0")
})


test_that("predict a single value with ml_ensemble", {
  m <- ml_model(m_mc_1) + ml_model(m_mc_2)
  test_data <- cbind(x=2, y=2)
  # expect warning because ensemble is not calibrated
  expect_warning(result <- predict(m, data=test_data))
  # result should be a matrix with class probabilities
  expect_equal(dim(result), c(1, 4))
  expect_equal(as.integer(which.max(result[1,])), 1)
  expect_equal(call_softmax(result), "label_0")
})


test_that("predict many values with ml_model", {
  m <- ml_model(m_mc_1)
  test_data <- cbind(x=c(2,2), y=c(2, -2))
  result <- predict(m, data=test_data)
  result_labels <- call_softmax(result)
  expect_equal(nrow(result), nrow(test_data))
  expect_false(result_labels[1] == result_labels[2])
})


test_that("predict many values with ml_model, too many features", {
  m <- ml_model(m_mc_1)
  test_data <- cbind(x=c(2,2), y=c(2, -2), z=c(0, 100))
  result_2 <- predict(m, data=test_data[, c("x", "y")])
  result_3 <- predict(m, data=test_data[, c("x", "y", "z")])
  expect_equal(nrow(result_2), nrow(test_data))
  # feature "z" does not play a role in the model
  expect_equal(result_2, result_3)
})


test_that("predict many values with ml_model, too few features", {
  m <- ml_model(m_mc_1)
  test_data <- cbind(x=c(2,2))
  result <- predict(m, data=test_data)
  # the model uses features x and y, here y is missing
  # but the model should still be able to predict something
  expect_equal(nrow(result), nrow(test_data))
})


test_that("predict many values with ml_ensemble", {
  m <- ml_model(m_mc_1) + ml_model(m_mc_2)
  test_data <- cbind(x=c(2,2), y=c(2, -2))
  # expect warning because ensemble is not calibrated
  expect_warning(result <- predict(m, data=test_data))
  # result should be a matrix with four columns
  expect_equal(dim(result), c(nrow(test_data), 4))
})


test_that("predict many values with ml_ensemble, using label names", {
  # m_mc_1 and m_mc_2 predict four classes/labels: 0,1,2,3
  abcd <- letters[1:4]
  m0 <- ml_model(m_mc_1) + ml_model(m_mc_2)
  m1 <- ml_model(m_mc_1, label_names=abcd) + ml_model(m_mc_2, label_names=abcd)
  test_data <- cbind(x=c(2,2), y=c(2, -2))
  # expect warning because ensemble is not calibrated
  expect_warning(result0 <- predict(m0, data=test_data))
  expect_warning(result1 <- predict(m1, data=test_data))
  # numeric results should be similar for the two ensembles
  expect_equal(dim(result0), dim(result1))
  expect_equal(result0[,1], result1[,1])
  # one of the ensembles should have column names, the other default names
  expect_equal(colnames(result0), paste0("label_", 0:3))
  expect_equal(colnames(result1), abcd)
})


test_that("predictions with ensemble can integrate different labels", {
  # models for d_mc_2 using labels 0,1,2,3 and 3,2,1,0
  mc1 <- m_mc_1
  mc2 <- xgboost(as.matrix(d_mc_2[, c("x", "y")]), label=d_mc_2$label,
                 nrounds=2, num_class=4, verbose=0,
                 objective="multi:softprob")
  mc2rev <- xgboost(as.matrix(d_mc_2[, c("x", "y")]), label=3-d_mc_2$label,
                    nrounds=2, num_class=4, verbose=0,
                    objective="multi:softprob")
  abcd <- letters[1:4]
  dcba <- rev(abcd)
  # create models without labels (model unlabeled -> mu)
  mu <- ml_model(mc1) + ml_model(mc2)
  murev <- ml_model(mc1) + ml_model(mc2rev)   # will likely confuse labels
  # create models with labels (model ensemble -> me)
  me <- ml_model(mc1, label_names=abcd) + ml_model(mc2, label_names=abcd)
  merev <- ml_model(mc1, label_names=abcd) + ml_model(mc2rev, label_names=dcba)
  test_data <- as.matrix(testdata_mc[seq(1, 120, by=3), c("x", "y")])
  test_labels <- testdata_mc$label[seq(1, 120, by=3)]
  # warnings because the the ensemble is not calibrated
  expect_warning(ru <- predict(mu, data=test_data))
  expect_warning(rurev <- predict(murev, data=test_data))
  expect_warning(re <- predict(me, data=test_data))
  expect_warning(rerev <- predict(merev, data=test_data))
  hit_rate <- function(x) {
    mean(apply(x, 1, which.max)-1 == test_labels)
  }
  #
  # results from mu, me should be comparable and quite good (>90%)
  expect_gt(hit_rate(ru), 0.9)
  expect_gt(hit_rate(re), 0.9)
  # rurev should be quite poor because labels will be confused (<80%)
  expect_lt(hit_rate(rurev), 0.8)
  # rerev should be good because labels should be matched (>90%)
  expect_gt(hit_rate(rerev), 0.9)
})


test_that("predictions with ensemble should be more accurate that ml_model", {
  m1 <- ml_model(m_mc_1)
  m2 <- ml_model(m_mc_2)
  me <- ml_model(m_mc_1) + ml_model(m_mc_2) + ml_model(m_mc_3)
  # construct a grid of data points, expected are quadrants
  test_data <- as.matrix(expand.grid(list(x=seq(-0.6, 0.6, by=0.1),
                                         y=seq(-0.6, 0.6, by=0.1))))
  test_data <- test_data[test_data[,1] != 0.0 & test_data[,2] != 0.0, ]
  expected <- rep(1, nrow(test_data))
  expected[test_data[,1]>0 & test_data[,2]<0] <- 2
  expected[test_data[,1]<0 & test_data[,2]<0] <- 3
  expected[test_data[,1]>0 & test_data[,2]>0] <- 4
  # compute predictions and errors
  p1 <- apply(predict(m1, data=test_data), 1, which.max)
  p2 <- apply(predict(m2, data=test_data), 1, which.max)
  expect_warning(pe <- apply(predict(me, data=test_data), 1, which.max))
  e1 <- sum(p1 != expected)
  e2 <- sum(p2 != expected)
  ee <- sum(pe != expected)
  # error for ensemble should be smaller than the worst of e1 and e2
  expect_lte(ee, max(e1, e2))
})


test_that("predictions using models that use different features", {
  mx <- ml_model(m_mc_x)   # uses only feature x
  my <- ml_model(m_mc_y)   # uses only feature y
  me <- ml_model(m_mc_x) + ml_model(m_mc_y)
  # construct a grid of data points, expected are quadrants
  test_data <- as.matrix(expand.grid(list(x=seq(-0.6, 0.6, by=0.1),
                                         y=seq(-0.6, 0.6, by=0.1))))
  test_data <- test_data[test_data[,1] != 0.0 & test_data[,2] != 0.0, ]
  expected <- rep(1, nrow(test_data))
  expected[test_data[,1]>0 & test_data[,2]<0] <- 2
  expected[test_data[,1]<0 & test_data[,2]<0] <- 3
  expected[test_data[,1]>0 & test_data[,2]>0] <- 4
  # compute predictions and errors
  px <- apply(predict(mx, data=test_data), 1, which.max)
  py <- apply(predict(my, data=test_data), 1, which.max)
  expect_warning(pe <- apply(predict(me, 1, data=test_data), 1, which.max))
  ex <- sum(px != expected)
  ey <- sum(py != expected)
  ee <- sum(pe != expected)
  # error for ensemble should be smaller than for one-feature models
  expect_lte(ee, max(ex, ey))
})


test_that("predictions with post-processing hooks", {
  left_right <- function(x) {
    cbind(left=x[, "Q4"]+x[, "Q3"], right=x[, "Q1"]+x[, "Q2"])
  }
  left_right_hook <- ml_hook(left_right, type="post")
  quadrants <- paste0("Q", 1:4)
  me0 <- ml_model(m_mc_x, label_names=quadrants) +
    ml_model(m_mc_y, label_names=quadrants)
  me1 <- ml_model(m_mc_x, label_names=quadrants, hooks=list(left_right_hook)) +
    ml_model(m_mc_y, label_names=quadrants, hooks=list(left_right_hook))
  # models within the ensemble should have hooks
  expect_equal(length(me1$models[[1]]$hooks), 1)
  calib_data <- copy(d_mc_3)
  calib_data$LR <- ifelse(d_mc_3$label %in% c(0, 1), 1, 0)
  # models have label names
  # so calibration with integer labels should not work
  expect_error(calibrate(me0, as.matrix(calib_data[, c("x", "y")]),
                         calib_data$label))
  # calibration with character-based labels should work
  mec0 <- calibrate(me0, as.matrix(calib_data[, c("x", "y")]),
                    quadrants[1+calib_data$label])
  mec1 <- calibrate(me1, as.matrix(calib_data[, c("x", "y")]),
                    c("left", "right")[1+calib_data$LR])
  # models within the calibrated ensemble should preserve hooks
  expect_equal(length(mec1$models[[1]]$hooks), 1)
  test_data <- as.matrix(testdata_mc[, c("x", "y")])
  test_labels <- testdata_mc$label
  p0 <- predict(mec0, test_data)
  p1 <- predict(mec1, test_data)
  expect_equal(ncol(p0), 4)
  expect_equal(ncol(p1), 2)
})

