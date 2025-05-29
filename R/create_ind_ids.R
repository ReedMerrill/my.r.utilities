#' new ID spec for 2nd round of CMB:
#' "year entered survey" + "_" + "3 serial #" + "_" + "census ID code"

create_ind_ids <- function(data, census_id_name, year) {
  # Split data by census_id groups
  grouped_data <- split(data, data[[census_id_name]])
  # Apply numbering within each group
  result <- do.call(
    rbind,
    lapply(grouped_data, function(group) {
      group$ind_id <- paste(
        year,
        sprintf("%03d", seq_len(nrow(group))), # Pad with leading zeros
        group[[census_id_name]],
        sep = "_"
      )
      group
    })
  )

  # Remove temporary column and restore original row order
  result$id_within_group <- NULL
  result <- result[order(as.numeric(rownames(result))), ]
  rownames(result) <- NULL

  return(result)
}
