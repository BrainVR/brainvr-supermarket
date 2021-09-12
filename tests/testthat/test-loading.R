BASE_PATH <- system.file("extdata", "raw", package = "brainvr.supermarket")

test_that("Loading version 1 works", {
  pth <- file.path(BASE_PATH, "version1")
  test_load_files(pth)
})

test_that("Loading version 2 works", {
  pth <- file.path(BASE_PATH, "version3")
  test_load_files(pth)
})

test_that("Loading version 3 works", {
  pth <- file.path(BASE_PATH, "version3")
  test_load_files(pth)
})

test_that("Loading version 4 works", {
  pth <- file.path(BASE_PATH, "version4")
  test_load_files(pth)
})

test_that("Loading version 5 works", {
  pth <- file.path(BASE_PATH, "version5")
  test_load_files(pth)
})

test_that("Loading version 6 works", {
  pth <- file.path(BASE_PATH, "version6")
  test_load_files(pth)
})

test_that("Loading with override works", {
  pth <- file.path(BASE_PATH, "version1")
  expect_error(exps <- load_supermarket_experiments(pth, override = TRUE), NA)
  expect_length(exps, 1)
  expect_s3_class(exps[[1]], "supermarket")
})

test_that("loading a single file works",{
  pth <- file.path(BASE_PATH, "version5")
  expect_error(exp <- load_supermarket_experiment(pth, "13-29-31-12-06-2020"), NA)
  expect_error(exp <- load_supermarket_experiment(pth, "13-29-31-12-06-2021"))
  expect_s3_class(exp, "supermarket")
})
