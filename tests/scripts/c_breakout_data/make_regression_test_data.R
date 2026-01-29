# A regression test uses real data to ensure that behaviour remains consistent
# over time. The `expected` data is manually verified to ensure it canonizes
# correct behaviour.

# NOTE: Don't edit this block. It prevents the entire script from being
# accidentally ran.
run <- FALSE
stopifnot(run)

source("../../../R/c_breakout.R")

# --------------------
# setup API call

# This API key needs to come from within the Qualtrics account of each person
# who runs this code.
api_key <- Sys.getenv("QUALTRICS_API_KEY")

# build the survey ID format required by qualtRics::fetch_survey. This is much
# faster than running the API call to Qualtrics using qualtRics::fetch_id and
# then extracting the id.
survey_id <- c("SV_3atxdt18a0ZeiPk")
names(survey_id) <- "CMB 2025/2026 - Politician"

qualtRics::qualtrics_api_credentials(
  api_key = api_key,
  base_url = "yul1.qualtrics.com",
  install = FALSE
)

df <- qualtRics::fetch_survey(
  surveyID = survey_id,
  include_display_order = FALSE,
  breakout_sets = FALSE,
  convert = FALSE,
  label = FALSE,
  add_column_map = TRUE,
  verbose = TRUE
)

colmap <- qualtRics::extract_colmap(df)

# --------------------
# fetch and output

fetch <- function(col_name, survey_id) {
  qid <- colmap |>
    dplyr::filter(qname == col_name) |>
    dplyr::pull(ImportId)

  input <- qualtRics::fetch_survey(
    surveyID = survey_id,
    include_display_order = FALSE,
    include_questions = qid,
    breakout_sets = TRUE,
    convert = FALSE,
    label = FALSE
  ) |>
    dplyr::select(tidyr::starts_with(col_name))

  expected <- input |> c_breakout(base_name = col_name, col = col_name)

  list("expected" = expected, "input" = input)
}

race <- fetch(col_name = "race", survey_id)

saveRDS(race$input, file = "../../testthat/data/c_breakout_input_race.rds")

saveRDS(
  race$expected,
  file = "../../testthat/data/c_breakout_expected_race.rds"
)

dp_support <- fetch(col_name = "dp_support", survey_id)

saveRDS(
  dp_support$input,
  file = "../../testthat/data/c_breakout_input_dp_support.rds"
)

saveRDS(
  dp_support$expected,
  file = "../../testthat/data/c_breakout_expected_dp_support.rds"
)
