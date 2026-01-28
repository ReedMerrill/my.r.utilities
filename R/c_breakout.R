#' @export
c_breakout <- function(data, pattern, col) {
  data <- data |>

    # Get the value of each breakout set member from the column name of its
    # dummy coded columns.
    dplyr::mutate(
      dplyr::across(
        dplyr::matches(pattern, perl = TRUE),
        ~ dplyr::if_else(
          .x == 1,
          stringr::str_extract(dplyr::cur_column(), "[0-9]+$"),
          ""
        )
      )
    ) |>

    # replace NA with "" in preparation for pasting together the values
    dplyr::mutate(
      dplyr::across(dplyr::matches(pattern), as.character),
      dplyr::across(
        dplyr::matches(pattern),
        ~ tidyr::replace_na(.x, "")
      )
    )

  # Paste values together. Each row contains a comma-separated list of
  # values chosen from the select-multiple question
  data <- data |>
    dplyr::rowwise() |>
    dplyr::mutate(
      !!col := paste(
        # select all columns that match `pattern`
        dplyr::c_across(dplyr::matches(pattern))[
          # exclude columns with empty strings
          dplyr::c_across(dplyr::matches(pattern)) != ""
        ],
        collapse = ","
      )
    ) |>
    dplyr::ungroup()

  data
}
