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

# --------------------
# Regression Tests

test_that("Regression - breakout sets concatenate (`race`)", {
  input <- readRDS("data/c_breakout_input_race.rds")
  expected <- readRDS("data/c_breakout_expected_race.rds")

  col_name <- "race"

  actual <- c_breakout(input, base_name = col_name, col = col_name)

  expect_equal(
    actual |> dplyr::select(all_of(col_name)),
    expected |> dplyr::select(all_of(col_name))
  )
})

test_that("Regression - breakout sets concatenate (`dp_support`)", {
  input <- readRDS("data/c_breakout_input_dp_support.rds")
  expected <- readRDS("data/c_breakout_expected_dp_support.rds")

  col_name <- "dp_support"

  actual <- c_breakout(
    input,
    base_name = col_name,
    col = col_name
  )

  expect_equal(
    actual |> dplyr::select(dplyr::all_of((col_name))),
    expected |> dplyr::select(dplyr::all_of(col_name))
  )
})
