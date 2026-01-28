# A regression test uses real data to ensure that behaviour remains consistent
# over time. The `expected` data is manually verified to ensure it canonizes
# correct behaviour.

# NOTE: Don't edit this block. It prevents the entire script from being
# accidentally ran.
run <- FALSE
stopifnot(run)

source("../../R/c_breakout.R")

# --------------------
# c_breakout: set up for API call

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
# c_breakout: race variable

race_qid <- colmap |>
  dplyr::filter(qname == "race") |>
  dplyr::pull(ImportId)

race_input <- qualtRics::fetch_survey(
  surveyID = survey_id,
  include_display_order = FALSE,
  include_questions = race_qid,
  breakout_sets = TRUE,
  convert = FALSE,
  label = FALSE
) |>
  dplyr::select(tidyr::starts_with("race"))

saveRDS(race_input, file = "../testthat/data/c_breakout_input_race.rds")

race_expected <- race_input |> c_breakout(pattern = "^race_\\d+$", col = "race")

saveRDS(race_expected, file = "../testthat/data/c_breakout_expected_race.rds")

# --------------------
# c_breakout: dp_support variable

dp_support_qid <- colmap |>
  dplyr::filter(qname == "dp_support") |>
  dplyr::pull(ImportId)

dp_support_raw <- qualtRics::fetch_survey(
  surveyID = survey_id,
  include_display_order = FALSE,
  include_questions = dp_support_qid,
  breakout_sets = TRUE,
  convert = FALSE,
  label = FALSE
)

saveRDS(
  dp_support_raw,
  file = "../testthat/data/c_breakout_input_dp_support.rds"
)

dp_support_clean <- dp_support_raw |>
  c_breakout(pattern = "^dp_support_\\d+$", col = "dp_support") |>
  dplyr::select(dp_support)

saveRDS(
  dp_support_clean,
  file = "../testthat/data/c_breakout_expected_dp_support.rds"
)
