# test displaying package object using print

# create some simple datasets and models
if (!exists("d_linear")) {
  source("datasets_models.R")
  source("helpers.R")
}


test_that("print for one hook displays name and type", {
  my_fun <- function(x) {
    x / apply(x, 1, sum)
  }
  my_hook <- ml_hook(my_fun, type="post")
  expect_message(print(my_hook), "ml_hook:")
  expect_message(print(my_hook), "  name")
  expect_message(print(my_hook), "my_fun")
  expect_message(print(my_hook), "post")
})


test_that("print for a list of hooks displays all names", {
  my_fun <- function(x) {
    x / apply(x, 1, sum)
  }
  h1 <- ml_hook(my_fun, name="h1")
  h2 <- ml_hook(my_fun, name="h2")
  hooks <- ml_hooks(list(h1, h2))
  expect_message(print(hooks), "ml_hooks")
  expect_message(print(hooks), "- name")
  expect_message(print(hooks), "h1")
  expect_message(print(hooks), "h2")
})


test_that("print for one ml_model displays name", {
  m1 <- ml_model(m_lm_1)
  expect_message(print(m1), "m_lm_1")
})


test_that("print for ml_ensemble displays number of models", {
  me <- ml_model(m_lm_1) + ml_model(m_lm_2)
  expect_message(print(me), "m_lm_1")
  expect_message(print(me), "m_lm_2")
})


