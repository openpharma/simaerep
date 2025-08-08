


df_site_max_test <- get_df_visit_test() %>%
  group_by(.data$study_id, .data$site_id, .data$patient_id) %>%
  summarise(max_visit = max(.data$visit),
            max_event = max(.data$n_event),
            .groups = "drop")

df_config_test <- simaerep::get_config(
  df_site_max_test,
  anonymize = TRUE,
  min_pat_per_study = 100,
  min_sites_per_study = 5
)

set.seed(1)

df_portf_test <- sim_test_data_portfolio(df_config_test, progress = FALSE)


test_that("get_config() - check column names of returned data frame", {

  expect_true(
    all(
      c("study_id", "event_per_visit_mean", "site_id", "max_visit_sd",
        "max_visit_mean", "n_pat") %in% colnames(df_config_test)
    )
  )
})

test_that("sim_test_data_portfolio() - check column names of returned data frame", {
  expect_true(
    all(
      c("study_id", "event_per_visit_mean", "site_id", "max_visit_sd",
        "max_visit_mean", "patient_id", "visit", "n_event") %in% colnames(df_portf_test)
    )
  )
})



