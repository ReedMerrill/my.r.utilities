exclude_text_breakout <- function(x) {
  text_cols <- grep("_TEXT$", x, value = TRUE)
  text_base_cols <- sub("_TEXT$", "", text_cols)

  x[!x %in% c(text_base_cols, text_cols)]
}

#' @export
c_breakout <- function(data, base_name, col) {
  all_pattern <- paste0("^", base_name, "_[0-9]+(_TEXT)?$")
  all_cols <- colnames(data) |> stringr::str_subset(all_pattern)
  selected_cols <- exclude_text_breakout(all_cols)

  data <- data |>

    # Get the value of each breakout set member from the column name of its
    # dummy coded columns.
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(selected_cols),
        ~ dplyr::if_else(
          .x == 1,
          stringr::str_extract(dplyr::cur_column(), "[0-9]+$"),
          ""
        )
      )
    ) |>

    # replace NA with "" in preparation for pasting together the values
    dplyr::mutate(
      dplyr::across(dplyr::all_of(selected_cols), as.character),
      dplyr::across(
        dplyr::all_of(selected_cols),
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
        dplyr::c_across(dplyr::all_of(selected_cols))[
          # exclude columns with empty strings
          dplyr::c_across(dplyr::all_of(selected_cols)) != ""
        ],
        collapse = ","
      )
    ) |>
    dplyr::ungroup()

  data
}
