# test displaying package objects using summary

# create some simple datasets and models
if (!exists("d_linear")) {
  source("datasets_models.R")
  source("helpers.R")
}


test_that("format a long line into several wrapped lines", {
  x <- "The quick brown fox jumped over the lazy dog."
  result <- wrapped_lines(x, line_len=14)
  expect_equal(length(result), 3)
})


test_that("describe a single model (plain)", {
  m <- ml_model(m_lm_1, name="custom")
  result <- suppressMessages(summary(m))
  expect_message(summary(m), "custom")
  expect_true(grepl("uses two features", result))
  expect_true(grepl("any post-processing steps", result))
})


test_that("describe a single model (with description)", {
  m <- ml_model(m_lm_1, name="custom", description="uses linear regression")
  expect_message(summary(m), "custom' uses linear regression\\.")
  expect_message(summary(m), "It uses two features")
})


test_that("describe a single model (with description and hooks)", {
  f1 <- function(x) {x}
  h1 <- ml_hook(f1, name="hook1", type="pre")
  h2 <- ml_hook(f1, name="hook2", type="pre")
  h3 <- ml_hook(f1, name="hook3", type="post")
  m <- ml_model(m_lm_1, name="custom", description="uses linear regression",
                 hooks=ml_hooks(list(h1, h2, h3)))
  result <- suppressMessages(summary(m))
  expect_message(summary(m), "custom' uses linear regression\\.")
  expect_true(grepl("It uses two features", result))
  expect_true(grepl("two pre-processing steps", result))
  expect_true(grepl("one post-processing step", result))
})


test_that("describe an ensemble (minimal)", {
  me <- ml_model(m_lm_1) + ml_model(m_lm_2)
  expect_message(summary(me), "two models:")
  expect_message(summary(me), "m_lm_1")
  expect_message(summary(me), "m_lm_2")
})


test_that("describe an ensemble (with description)", {
  me0 <- ml_ensemble(name="my_ensemble",
                     description="integrates predictions from many datasets")
  me1 <- me0 + ml_model(m_lm_1) + ml_model(m_lm_2)
  result <- suppressMessages(summary(me1))
  expect_message(summary(me1), ".my_ensemble. integrates")
  expect_true(grepl("The ensemble consists of two models", result[1]))
  expect_true(grepl("m_lm_1", result[2]))
  expect_true(grepl("m_lm_2", result[3]))
})


test_that("describe a calibrated ensemble", {
  me0 <- ml_ensemble() + ml_model(m_mc_pop0)
  calib_data <- data.table(cbind(x=rnorm(20), y=rnorm(20)))
  calib_data$label <- rep(c(0, 1), nrow(calib_data)/2)
  me1 <- suppressWarnings(calibrate(me0,
                                    data=as.matrix(calib_data[, c("x", "y")]),
                                    label=calib_data$label))
  expect_message(summary(me0), "not calibrated")
  expect_message(summary(me1), "calibrated with a dataset of 20 items")
})


test_that("describe an ensemble that has a nested ensemble", {
  me <-  ml_model(m_lm_1) + ml_ensemble("ens")
  result <- suppressMessages(summary(me))
  # the description should not describe anything as a 'list'
  expect_false(any(grepl("list", result)))
  # expected paragraph structure: intro, description of m_lm_1,
  # description of ens, ending paragraph
  expect_true(grepl("lm", result[2]))
  # descriptions of the ensemble should mention ensemble, but not list features
  expect_true(grepl("ensemble", result[3]))
  expect_false(grepl("features", result[3]))
})

