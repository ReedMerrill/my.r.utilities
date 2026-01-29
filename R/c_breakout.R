exclude_text_breakout <- function(x) {
  text_cols <- grep("_TEXT$", x, value = TRUE)
  text_base_cols <- sub("_TEXT$", "", text_cols)

  x[!x %in% c(text_base_cols, text_cols)]
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
