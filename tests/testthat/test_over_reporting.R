

test_that(paste("prob_lower_site_ae_vs_study_ae() - high number of AEs at site",
                "compared to study, expect prob_low < 1 when under_only = FALSE"), {

    prob_low <- prob_lower_site_ae_vs_study_ae(
      site_ae = c(9, 8, 7, 9, 6, 7, 8),
      study_ae = c(5, 3, 3, 2, 1, 6),
      under_only = FALSE
    )

    expect_true(prob_low < 1)

})


test_that("sim_test_data_study() - positive values passed to factor_event_rate simulate over-reporting", {

  ae_per_visit_mean_def <- 0.5

  df_visit <- sim_test_data_study(
    factor_event_rate = +0.5,
    ratio_out = 0.05,
    event_rates = ae_per_visit_mean_def,
    event_names = "ae"
  )

  ae_per_visit_mean_or <- df_visit %>%
    filter(is_out) %>%
    pull(ae_per_visit_mean) %>%
    unique()

  expect_true(ae_per_visit_mean_or > ae_per_visit_mean_def)

})



test_that(paste("simaerep() - under_only = FALSE - over-reporting must be zero when",
                "mean_ae_site_med75 == mean_ae_study_med75"), {

  df_visit <- sim_test_data_study(
    event_rates = 0
  ) %>%
  mutate(study_id = "A")

  aerep <- simaerep(df_visit, under_only = FALSE)

  zeros <- aerep$df_eval %>%
    pull(event_prob)

  expect_true(all(zeros == 0))

})
