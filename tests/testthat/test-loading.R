context("Loading")

BASE_PATH <- system.file("extdata", "raw", package="brainvr.supermarket")

test_that("Loading version 1 works as intended", {
  pth <- file.path(BASE_PATH, "version1")
  test_load_files(pth)
})

test_that("Loading version 2 works as intended", {
  pth <- file.path(BASE_PATH, "version3")
  test_load_files(pth)
})

test_that("Loading version 3 works as intended", {
  pth <- file.path(BASE_PATH, "version3")
  test_load_files(pth)
})

test_that("Loading version 4 works as intended", {
  pth <- file.path(BASE_PATH, "version4")
  test_load_files(pth)
})
