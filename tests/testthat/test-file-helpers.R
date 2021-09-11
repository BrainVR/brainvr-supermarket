BASE_PATH <- system.file("extdata", "raw", package = "brainvr.supermarket")
pth_cleaning <- file.path(BASE_PATH, "version5-wrong")
pth_test <- "test-temp"

test_that("test files can be cleaned", {
  fs::dir_copy(pth_cleaning, pth_test)
  clear_test_logs(pth_test)
  expect_length(list.files(pth_test, pattern = "_test_"), 0)
  withr::defer(fs::dir_delete(pth_test))
})

test_that("actions can be renamed", {
  fs::dir_copy(pth_cleaning, pth_test)
  rename_action_to_test_logs(pth_test)
  withr::defer(fs::dir_delete(pth_test))
})
