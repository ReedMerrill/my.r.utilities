input <- readRDS("data/c_breakout_input.rds")
expected <- readRDS("data/c_breakout_expected.rds")

output_col_name <- "test"

actual <- c_breakout(input, pattern = "^race_\\d+$", col = output_col_name)

test_that("Breakout sets concatenate", {
  expect_equal(
    actual |> dplyr::pull(output_col_name),
    expected
  )
})
