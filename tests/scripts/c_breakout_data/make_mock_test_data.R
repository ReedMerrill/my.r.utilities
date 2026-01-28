make_mock_test_data <- function() {
  set.seed(1)

  vals <- function(prob) {
    sample(c(1, NA_integer_), size = 20, replace = TRUE, prob = prob)
  }

  strings <- function() {
    sample(c("A", "B", "C", "D"), 1)
  }

  input <- tibble::tibble(
    col_1 = vals(c(0.05, 0.95)),
    col_2 = vals(c(0.05, 0.95)),
    col_3 = vals(c(0.05, 0.95)),
    col_4 = vals(c(0.05, 0.95)),
    col_5 = vals(c(0.05, 0.95)),
    col_6 = vals(c(0.05, 0.95)),
    col_7 = vals(c(0.75, 0.25)),
    col_8 = vals(c(0.3, 0.7))
  ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      col_8_TEXT = dplyr::if_else(
        condition = !is.na(col_8),
        true = strings(),
        false = NA_character_
      )
    ) |>
    dplyr::ungroup()

  expected <- tibble::tibble(
    "col" = c(
      "",
      "7",
      "7",
      "6,7",
      "",
      "7",
      "7",
      "7",
      "7",
      "7",
      "6,7",
      "7",
      "7",
      "7",
      "",
      "7",
      "7",
      "1,7",
      "",
      "4,7"
    )
  )

  list("input" = input, "expected" = expected)
}
