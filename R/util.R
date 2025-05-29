# print the unique values of each variable in a data set
#' @export
variable_checker <- function(data, exclude_cols = c()) {
  col_i <- 1:ncol(data)

  cols <- data |> colnames()

  col_names_tbl <- tibble::tibble(cols, col_i)

  col_names_tbl <- col_names_tbl |> dplyr::filter(!(cols %in% exclude_cols))

  for (i in 1:nrow(col_names_tbl)) {
    print(paste("Column #:", col_names_tbl$cols[i]))
    print(unique(data[[col_names_tbl$cols[i]]]))
  }
}

# return a char vector of columns based on a pattern
#' @export
find_col_group <- function(dt, pattern) {
  cols_vec <- dt |>
    dplyr::select(tidyselect::matches(pattern)) |>
    colnames()

  return(cols_vec)
}

# Coalesces data based on a pattern that duplicate columns start with
#' @export
coalesce_across <- function(dt, pattern) {
  cols_vec <- dt |>
    dplyr::select(tidyselect::starts_with(pattern)) |>
    colnames()

  # !!! unpacks a vector
  # !! forces part of the expression to be evaluated first
  dt <- dt |>
    dplyr::mutate(!!pattern := dplyr::coalesce(!!!rlang::syms(cols_vec))) # nolint

  return(dt)
}

# concatenate character vectors of related variables into a single vector
#' @export
str_concat_across <- function(dt, pattern) {
  cols_vec <- dt |>
    dplyr::select(tidyselect::starts_with(pattern)) |>
    colnames()

  dt <- dt |>
    dplyr::mutate(!!pattern := paste(!!!rlang::syms(cols_vec), sep = ", ")) # nolint

  return(dt)
}

#' Recode a target column dynamically
#'
#' This function recodes a target column dynamically using values from a range
#' of related columns
#' @param data A data.frame, tibble, etc.
#' @param target_col The name of the column to be recoded.
#' @param source_col_pattern A regex to match the source columns to be used in
#' the recode.
#' @param na_vals Values to be ignored.
#' @return None. This function is called for its side-effects only. The `target`
#' column is recoded.
#' @export
recode_from_embedded <- function(
  data,
  target_col,
  source_col_pattern,
  na_vals = c("997", "998", "999")
) {
  data[["tmp_col"]] <- NA

  for (i in seq_len(nrow(data))) {
    if (
      !(data[[target_col]][i] %in% na_vals) && !is.na(data[[target_col]][i])
    ) {
      filter <- paste0(
        source_col_pattern, # the column name prefix
        data[[target_col]][i]
      ) # the value of the target col (matches the suffix of the source col)

      data[["tmp_col"]][i] <- data[[filter]][i]
    } else {
      data[["tmp_col"]][i] <- data[[target_col]][i]
    }
  }

  data <- data |>
    dplyr::select(-tidyselect::all_of(target_col)) |>
    dplyr::rename(!!target_col := "tmp_col") # nolint

  return(data)
}
