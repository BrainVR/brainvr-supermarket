test_load_files <- function(pth) {
  expect_error(exps <- load_supermarket_experiments(pth), NA)
  expect_length(exps, 1)
  expect_s3_class(exps[[1]], "supermarket")
}
