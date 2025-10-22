

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


test_that(paste("no over-reporting for sites with zero events"), {

  # https://github.com/openpharma/simaerep/issues/92

  sites <- 10
  pat <- 5
  vis_m <- 15

  df_vis <- data.frame(
    patnum = paste0("P", rep(1:50, each = 15)),
    site_number = paste0("S", rep(1:10, each = 75)),
    visit = rep(1:15, times = 50),
    n_event = 0,
    study_id = "A"
  )

  df_vis[15, ]$n_event <- 1

  zero_rep_prob <- simaerep(df_vis, inframe = TRUE)$df_eval %>%
    filter(event_count == 0) %>%
    pull(event_prob)

  expect_true(all(zero_rep_prob <= 0.6))

  # classic algorithm not changed
  zero_rep_prob <- simaerep(df_vis, inframe = FALSE)$df_eval %>%
    filter(mean_event_site_med75 == 0) %>%
    pull(prob)

  expect_false(all(zero_rep_prob <= 0.6))

})
