

test_that(paste("prob_lower_site_ae_vs_study_ae() - high number of AEs at site",
                "compared to study, expect prob_low < 1 when under_only = FALSE"), {

                  prob_low <- prob_lower_site_ae_vs_study_ae(
                    site_ae = c(9, 8, 7, 9, 6, 7, 8),
                    study_ae = c(5, 3, 3, 2, 1, 6),
                    under_only = FALSE
                  )

                  expect_true(prob_low < 1)

})


test_that("sim_test_data_study() - negative values passed to ur_rate simulate over-reporting", {

  ae_per_visit_mean_def <- 0.5

  df_visit <- sim_test_data_study(
    ur_rate = - 0.5,
    frac_site_with_ur = 0.05,
    ae_per_visit_mean = ae_per_visit_mean_def
  )

  ae_per_visit_mean_or <- df_visit %>%
    filter(is_ur) %>%
    pull(ae_per_visit_mean) %>%
    unique()

  expect_true(ae_per_visit_mean_or > ae_per_visit_mean_def)

})


test_that("simaerep() - under_only = FALSE return over-reporting statistics", {

  aerep <- simaerep(df_visit_test, under_only = FALSE)

  expect_true(all(
    c("prob_high", "prob_high_adj", "prob_high_prob_or") %in% colnames(aerep$df_eval)
  ))

})

test_that(paste("simaerep() - under_only = FALSE - over-reporting must be zero when",
                "mean_ae_site_med75 == mean_ae_study_med75"), {

  df_visit <- sim_test_data_study(
    ae_per_visit_mean = 0
  ) %>%
  mutate(study_id = "A")

  aerep <- simaerep(df_visit, under_only = FALSE)

  zeros <- aerep$df_eval %>%
    filter(mean_ae_study_med75 == mean_ae_site_med75) %>%
    summarise(across(c(prob_low_prob_ur, prob_high_prob_or), sum)) %>%
    summarise(across(c(prob_low_prob_ur, prob_high_prob_or), ~ . == 0)) %>%
    unlist()

  expect_true(all(zeros))

})
