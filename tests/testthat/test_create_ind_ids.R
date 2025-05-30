tbl <- readr::read_csv("data/test-data.csv")

ids_expected <- c(
  "2025_001_4701024",
  "2025_002_4701024",
  "2025_003_4701024",
  "2025_001_4715094",
  "2025_002_4715094",
  "2025_003_4715094",
  "2025_004_4715094",
  "2025_001_NULL"
)

out <- create_ind_ids(tbl, "census_id", 2025)

cols_diff <- setdiff(colnames(out), colnames(tbl))

test_that("`create_ind_ids` only creates one new column", {
  expect_equal(length(cols_diff), 1)
})

test_that("Individual ID column is called \"ind_id\"", {
  expect_equal(cols_diff[1], "ind_id")
})

test_that("Individual IDs are formatted correctly", {
  expect_equal(out$ind_id, ids_expected)
})
