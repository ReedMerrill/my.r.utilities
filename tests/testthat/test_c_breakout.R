# --------------------
# Unit Tests

source("../scripts/c_breakout_data/make_mock_test_data.R")

test_that(
  paste(
    "<base_num_TEXT> column is",
    "present and excludes its dummy coded column"
  ),
  {
    input <- c("col_1", "col_2", "col_3", "col_3_TEXT")
    expected <- c("col_1", "col_2")
    actual <- exclude_text_breakout(input)
    expect_equal(actual, expected)
  }
)

test_that(
  paste(
    "c_breakout produces a concatenated column of the dummy coded",
    "(breakout) data that restores the original values"
  ),
  {
    res <- make_mock_test_data()
    input <- res$input
    expected <- res$expected

    actual <- c_breakout(input, base_name = "col", col = "col")

    expect_equal(actual |> dplyr::select(col), expected)
  }
)
#
# # --------------------
# # Regression Tests
#
# test_that("Breakout sets concatenate (race)", {
#   input <- readRDS("data/c_breakout_input_race.rds")
#   expected <- readRDS("data/c_breakout_expected_race.rds")
#
#   output_col_name <- "race"
#
#   actual <- c_breakout(input, pattern = "^race_\\d+$", col = output_col_name)
#
#   expect_equal(
#     actual |> dplyr::select(dplyr::all_of(output_col_name)),
#     expected
#   )
#
#   expect_equal(
#     actual |> dplyr::select(dplyr::all_of(output_col_name)) |> colnames(),
#     expected |> colnames(),
#     label = "actual column name"
#   )
# })
#
# test_that("Breakout sets concatenate (dp_support)", {
#   input <- readRDS("data/c_breakout_input_dp_support.rds")
#   expected <- readRDS("data/c_breakout_expected_dp_support.rds")
#
#   output_col_name <- "dp_support"
#
#   actual <- c_breakout(
#     input,
#     pattern = "^dp_support_\\d+$",
#     col = output_col_name
#   )
#
#   expect_equal(
#     actual |> dplyr::select(dplyr::all_of((output_col_name))),
#     expected
#   )
# })
#
# test_that(
#   paste(
#     "The text response flag column from the breakout",
#     "set isn't used in c_breakout"
#   ),
#   {
#     expect_equal(
#       0,
#       1
#     )
#   }
# )
#
# test_that(
#   paste(
#     "Only breakout set columns are dropped, and",
#     "<name_value_TEXT> type text response columns are kept"
#   ),
#   {
#     expect_equal(
#       actual |> dplyr::pull(output_col_name),
#       expected
#     )
#   }
# )
