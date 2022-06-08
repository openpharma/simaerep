# test data is automatically loaded, check ./data-raw/generate_test_data.R

test_that("check S3 test data reproducibility", {
  aerep_check <- simaerep(
    df_visit_test,
    param_sim_sites = list(
      r = 100
    )
  )

  expect_equal(aerep_check, aerep_test)

  visit_check <- orivisit(df_visit_test)

  expect_equal(visit_check, visit_test)

})

test_that("check standard test data reproducibility", {

  set.seed(1)

  df_visit1 <- sim_test_data_study(
    n_pat = 100,
    n_sites = 5,
    frac_site_with_ur = 0.4,
    ur_rate = 0.6
  )

  df_visit1$study_id <- "A"

  set.seed(2)
  df_visit2 <- sim_test_data_study(n_pat = 100, n_sites = 5,
                                   frac_site_with_ur = 0.2, ur_rate = 0.1)

  df_visit2$study_id <- "B"

  df_visit_check <- dplyr::bind_rows(df_visit1, df_visit2)

  aerep <- simaerep(
    df_visit_check,
    param_sim_sites = list(
      poisson_test = TRUE,
      r = 100
      )
  )

  expect_equal(df_visit_test, df_visit_check)
  expect_equal(df_site_test, aerep$df_site)
  expect_equal(df_sim_sites_test, aerep$df_sim_sites)
  expect_equal(df_eval_test, aerep$df_eval)

})


test_that("check portfolio performance test data reproducibility", {

  df_site_max_check <- df_visit_test %>%
    group_by(study_id, site_number, patnum) %>%
    summarise(max_visit = max(visit),
              max_ae = max(n_ae),
              .groups = "drop")

  df_config_check <- simaerep::get_config(
    df_site_max_check,
    anonymize = TRUE,
    min_pat_per_study = 100,
    min_sites_per_study = 5
  )

  set.seed(1)
  df_portf_check <- sim_test_data_portfolio(df_config_check, progress = FALSE)

  set.seed(1)
  df_scen_check <- sim_ur_scenarios(
    df_portf_check,
    extra_ur_sites = 2,
    ur_rate = c(0.5, 1),
    parallel = FALSE,
    poisson = FALSE,
    prob_lower = TRUE,
    progress = FALSE,
    site_aggr_args = list(method = "med75_adj")
  )

  df_perf_check <- get_portf_perf(df_scen_check)

  expect_equal(df_site_max_test, df_site_max_check)
  expect_equal(df_config_test, df_config_check)
  expect_equal(df_portf_test, df_portf_check)
  expect_equal(df_scen_test, df_scen_check)
  expect_equal(df_perf_test, df_perf_check)
})
