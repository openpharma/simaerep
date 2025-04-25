
df_portf <- get_df_portf_test()

df_scen <- sim_ur_scenarios(
  df_portf_test,
  extra_ur_sites = 2,
  ur_rate = c(0.5, 1),
  parallel = FALSE,
  poisson = FALSE,
  prob_lower = TRUE,
  progress = FALSE,
  site_aggr_args = list(method = "med75_adj")
)

df_perf <- get_portf_perf(df_scen)


test_that("get_config() - check column names of returned data frame", {

  df_config <- simaerep::get_config(
    df_site_max_test,
    anonymize = TRUE,
    min_pat_per_study = 100,
    min_sites_per_study = 5
  )

  expect_true(
    all(
      c("study_id", "ae_per_visit_mean", "site_number", "max_visit_sd",
        "max_visit_mean", "n_pat") %in% colnames(df_config)
    )
  )
})

test_that("sim_test_data_portfolio() - check column names of returned data frame", {
  expect_true(
    all(
      c("study_id", "ae_per_visit_mean", "site_number", "max_visit_sd",
        "max_visit_mean", "patnum", "visit", "n_ae") %in% colnames(df_portf)
    )
  )
})

test_that("sim_ur_scenarios() - check column names of returned data frame", {

  expect_true(
    all(
      c("study_id", "site_number", "n_pat", "n_pat_with_med75",
        "visit_med75", "mean_ae_site_med75", "mean_ae_study_med75",
        "n_pat_with_med75_study", "extra_ur_sites", "frac_pat_with_ur",
        "ur_rate", "prob") %in% colnames(df_scen)
    )
  )
})


test_that("get_portf_perf() - check column names of returned data frame", {
  expect_true(
    all(
      c("fpr", "thresh", "extra_ur_sites", "ur_rate",
        "tpr") %in% colnames(df_perf)
    )
  )
})

test_that("get_portf_perf() must throw warning when NA values encountered", {

  df_scen_na <- df_scen %>%
    dplyr::bind_rows(
      df_scen %>%
        mutate(prob = NA,
               site_number = paste("A", site_number))
    )

  expect_warning(get_portf_perf(df_scen_na))

})
