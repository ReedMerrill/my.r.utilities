library(testthat)

# Mock data for testing
test_df <- data.frame(
  a = c(1, NA, NA, 1),
  b = c(2, 5, NA, NA),
  c = c(NA, 6, NA, 3),
  stringsAsFactors = FALSE
)

test_that("last_non_na_col returns correct column names", {
  expected_names <- c("b", "c", NA_character_, "c")

  # Test default behavior (return_index = FALSE)
  expect_equal(last_non_na_col(test_df), expected_names)
  expect_type(last_non_na_col(test_df), "character")
})

test_that("last_non_na_col returns correct column indices", {
  expected_indices <- c(2, 3, NA_integer_, 3)

  # Test index behavior
  expect_equal(last_non_na_col(test_df, return_index = TRUE), expected_indices)
  expect_type(last_non_na_col(test_df, return_index = TRUE), "integer")
})

test_that("last_non_na_col handles edge cases", {
  # Single row data frame
  single_row <- data.frame(a = 1, b = NA)
  expect_equal(last_non_na_col(single_row), "a")

  # Row with only one non-NA at the very start
  start_only <- data.frame(a = 1, b = NA, c = NA)
  expect_equal(last_non_na_col(start_only, return_index = TRUE), 1)

  # Data frame with all NAs
  all_na <- data.frame(a = NA_real_, b = NA_real_)
  expect_true(is.na(last_non_na_col(all_na)[1]))
})
