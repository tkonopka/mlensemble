# test construction of hooks


test_that("create a simple hook", {
  my_hook <- ml_hook(rowsum_norm, type="post")
  expect_is(my_hook, "ml_hook")
  expect_is(my_hook$hook_fun, "function")
  expect_equal(my_hook$hook_name, "rowsum_norm")
  expect_equal(my_hook$hook_type, "post")
})


test_that("ml_hook checks hook name", {
  expect_error(ml_hook(function(x) { x }, type="post"))
  expect_error(ml_hook(rowsum_norm, name="a b c"))
  expect_error(ml_hook(rowsum_norm, name="a\nb\tc"))
})


test_that("get hooks in order (pre and post)", {
  h1 <- ml_hook(rowsum_norm, "h1", type="pre", order=2)
  h2 <- ml_hook(rowsum_norm, "h2", type="post", order=3)
  h3 <- ml_hook(rowsum_norm, "h3", type="pre", order=1)
  hooks <- ml_hooks(list(h1, h2, h3))
  result_pre <- get_hooks(hooks, type="pre")
  result_post <- get_hooks(hooks, type="post")
  # there should be one post hook
  expect_equal(length(result_post), 1)
  expect_equal(result_post[[1]]$hook_name, "h2")
  # there should be two pre hooks, and the one with small order should be first
  expect_equal(length(result_pre), 2)
  expect_equal(result_pre[[1]]$hook_name, "h3")
  expect_equal(result_pre[[2]]$hook_name, "h1")
})


test_that("get hooks in order (no post hooks)", {
  h1 <- ml_hook(rowsum_norm, "h1", type="pre")
  h3 <- ml_hook(rowsum_norm, "h3", type="pre")
  hooks <- ml_hooks(list(h1, h3))
  result_pre <- get_hooks(hooks, type="pre")
  result_post <- get_hooks(hooks, type="post")
  # there should not be any post hook
  expect_equal(length(result_post), 0)
  # there should be two pre hooks, and the one with small order should be first
  expect_equal(length(result_pre), 2)
  expect_equal(result_pre[[1]]$hook_name, "h1")
  expect_equal(result_pre[[2]]$hook_name, "h3")
})


test_that("get hooks when there aren't any", {
  hooks <- ml_hooks()
  result_pre <- get_hooks(hooks, type="pre")
  result_post <- get_hooks(hooks, type="post")
  expect_equal(length(result_post) + length(result_pre), 0)
})
