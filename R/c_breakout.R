#` c_breakout
#`
#` Given a vector of column names from `qualtRics` export with `breakout_sets =
#` TRUE`, filter the list to exclude the "_TEXT" columns and their base columns.
#`
#` @details
#` When data is exported with `qualtRics` and breakout_sets = TRUE, you get
#` dummy/one-hot encoded columns for each option in a select-multiple question.
#` If there is a text entry option, there will be two columns for it: one with
#` the numeric suffix of the option and one with the "_TEXT" suffix, e.g.
#` "col_7" and "col_7_TEXT".
#`
#` This function takes a vector of column names and excludes the "_TEXT" columns
#` and their base columns, leaving only the dummy coded columns for the breakout
#` set.
#`
#` @param col_names A character vector of column names.
#` @return A character vector of column names with the "_TEXT" columns and
#` their base (dummy indicator) columns excluded.
exclude_text_breakout <- function(col_names) {
  text_cols <- grep("_TEXT$", col_names, value = TRUE)
  text_base_cols <- sub("_TEXT$", "", text_cols)

  col_names[!col_names %in% c(text_base_cols, text_cols)]
}

# Test if a value in a column is truthy in a dichotomous coding setup.
# Returns true if is it, false if it isn't, and stops execution if it doesn't
# handle vec's type.
is_truthy <- function(vec) {
  if (is.logical(vec)) {
    return(vec %in% TRUE)
  }
  if (is.numeric(vec)) {
    return(vec %in% 1)
  }
  if (is.character(vec)) {
    return(tolower(vec) %in% "1")
  }

  stop("Unhandled dummy encoding type: ", typeof(vec))
}

#' Collapse Qualtrics breakout set columns into a single comma-separated column
#'
#' Reconstruct a select-multiple response exported from `qualtRics` with
#' `breakout_sets = TRUE` by collapsing its dummy-coded columns into a single
#' comma-separated character column.
#'
#' @details
#' When data are exported from `qualtRics` with `breakout_sets = TRUE`,
#' each response option in a select-multiple question is represented as a
#' separate dummy/one-hot encoded column (e.g., `"Q1_1"`, `"Q1_2"`).
#' If an option allows text entry, two columns are created for that option:
#' one dummy column (e.g., `"Q1_7"`) and one text column (e.g.,
#' `"Q1_7_TEXT"`). The numeric values following the base name (e.g., `"Q1"`) in
#' the column names represent the value codes from the original select-multiple
#' question.
#'
#' This function:
#' \itemize{
#'   \item Selects all columns in `data` matching `base_name` followed by a
#'   numeric suffix (and optional `"_TEXT"` suffix).
#'   \item Excludes `"_TEXT"` columns and their corresponding base dummy
#'   columns.
#'   \item Tests each remaining dummy column for "truthy" values (logical
#'   `TRUE`, numeric `1`, or character `"1"`).
#'   \item Extracts the numeric suffix from each selected column name.
#'   \item Collapses selected value codes from the column names row-wise into
#`   a single comma-separated string stored in `col`.
#' }
#'
#' The result is a character vector where each row contains the numeric
#' codes of selected options, separated by commas.
#'
#' @param data A data frame containing breakout set columns.
#' @param base_name A character scalar giving the common prefix of the
#'   breakout set columns (e.g., `"Q1"`).
#' @param col A character scalar giving the name of the new column to create.
#'
#' @return A data frame with all original columns and an additional character
#'   column named `col` containing comma-separated selected option codes.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   Q1_1 = c(1, 0),
#'   Q1_2 = c(0, 1),
#'   Q1_3 = c(1, 1),
#'   Q1_3_TEXT = c("Other text", "")
#' )
#'
#' c_breakout(df, base_name = "Q1", col = "Q1_collapsed")
#' }
#'
#' @export
c_breakout <- function(data, base_name, col) {
  all_pattern <- paste0("^", base_name, "_[0-9]+(_TEXT)?$")
  all_cols <- colnames(data) |> stringr::str_subset(all_pattern)
  selected_cols <- exclude_text_breakout(all_cols)

  if (length(selected_cols) == 0) {
    stop("No matching columns in data")
  }

  if (col %in% names(data)) {
    stop("Overwriting existing column: ", col)
  }

  data |>

    # Get the value of each breakout set member from the column name of its
    # dummy coded columns.
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(selected_cols),
        ~ dplyr::if_else(
          # Test each value of each selected column for truthyness
          condition = is_truthy(.x),
          # recoded to the numeric suffix of the column's name
          true = stringr::str_extract(dplyr::cur_column(), "[0-9]+$"),
          false = "",
          missing = ""
        )
      )
    ) |>

    # Paste values together. Each row contains a comma-separated list of
    # values chosen from the select-multiple question
    dplyr::rowwise() |>
    dplyr::mutate(
      !!col := paste(
        # work on selected columns
        dplyr::c_across(dplyr::all_of(selected_cols))[
          # don't paste in a cell if has an empty string
          dplyr::c_across(dplyr::all_of(selected_cols)) != ""
        ],
        collapse = ","
      )
    ) |>
    dplyr::ungroup()
}
