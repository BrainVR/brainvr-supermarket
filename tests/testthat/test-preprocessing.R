test_that("Converting items to categories",{
  items <- c("ITEM_BALLONS", "ITEM_BALLONS", "ITEM_FRAME")
  expect_silent(categories <- convert_items_to_categories(items))
  expect_length(categories, length(items))

  items <- c("ITEM_NOT_EXISTENT", "ITEM_BALLONS", "ITEM_FRAME")
  expect_silent(categories <- convert_items_to_categories(items))
  expect_length(categories, length(items))
})
