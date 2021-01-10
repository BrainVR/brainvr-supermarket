BASE_PATH <- system.file("extdata", "raw", package = "brainvr.supermarket")
exps <- load_supermarket_experiments(file.path(BASE_PATH, "version6/"))
exp <- exps[[1]]

test_that("Trial analysis", {
  expect_silent(res <- supermarket_performance_trial(exp, 1))
  expect_type(res, "list")
  # TODO add checking for correct fields

})

test_that("Session analysis", {
  expect_silent(res <- supermarket_performance_all(exp))
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 4)
})
