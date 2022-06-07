# test data is automatically loaded, check ./data-raw/generate_test_data.R


test_that("get_config() - check column names of returned data frame", {
  expect_true(
    all(
      c("study_id", "ae_per_visit_mean", "site_number", "max_visit_sd",
        "max_visit_mean", "n_pat") %in% colnames(df_config_test)
    )
  )
})

test_that("sim_test_data_portfolio() - check column names of returned data frame", {
  expect_true(
    all(
      c("study_id", "ae_per_visit_mean", "site_number", "max_visit_sd",
        "max_visit_mean", "patnum", "visit", "n_ae") %in% colnames(df_portf_test)
    )
  )
})

test_that("sim_ur_scenarios() - check column names of returned data frame", {
  expect_true(
    all(
      c("study_id", "site_number", "n_pat", "n_pat_with_med75",
        "visit_med75", "mean_ae_site_med75", "mean_ae_study_med75",
        "n_pat_with_med75_study", "extra_ur_sites", "frac_pat_with_ur",
        "ur_rate", "prob_low", "prob_low_adj", "prob_low_prob_ur") %in% colnames(df_scen_test)
    )
  )
})


test_that("get_portf_perf() - check column names of returned data frame", {
  expect_true(
    all(
      c("fpr", "thresh", "extra_ur_sites", "ur_rate",
        "tpr") %in% colnames(df_perf_test)
    )
  )
})

test_that("get_portf_perf() must throw warning when NA values encountered", {

  df_scen_na <- df_scen_test %>%
    bind_rows(
      df_scen_test %>%
        mutate(prob_low_prob_ur = NA,
               site_number = paste("A", site_number))
    )

  expect_warning(get_portf_perf(df_scen_na))

})
