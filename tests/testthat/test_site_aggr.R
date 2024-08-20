# test data is automatically loaded, check ./data-raw/generate_test_data.R


test_that("site_aggr() must not return a grouped dataframe", {
  expect_false(is_grouped_df(df_site_test))
})


test_that("site_aggr() - check column names of returned data frame", {
  df_site <- site_aggr(df_visit_test)

  expect_true(all(c(
    "study_id",
    "site_number",
    "n_pat",
    "n_pat_with_med75",
    "visit_med75",
    "mean_ae_site_med75"
  ) %in% names(df_site)))
})

test_that("site_aggr() must return different results for method = 'med75_adj' and method = 'med75'", {
  # default
  df_site_adj <- site_aggr(df_visit_test, method = "med75_adj")

  # deprecated
  df_site_old <- site_aggr(df_visit_test, method = "med75")

  expect_false(identical(df_site_adj, df_site_old))
  expect_equal(df_site_adj, df_site_test)
})


test_that("site_aggr() - visitmed75 must not become greater than 0.8 percentile of the max visit of all patients", {
  set.seed(1)

  # create three small sites with early and late starting patients
  df_visit1 <- sim_test_data_study(
    n_pat = 18,
    n_sites = 3,
    frac_site_with_ur = 0.4,
    ur_rate = 0.2,
    max_visit_sd = 2,
    max_visit_mean = 20
  )

  df_visit2 <- sim_test_data_study(
    n_pat = 9,
    n_sites = 3,
    frac_site_with_ur = 0.4,
    ur_rate = 0.2,
    max_visit_sd = 2,
    max_visit_mean = 5
  )


  df_visit1$patnum <- paste0("A", df_visit1$patnum)
  df_visit2$patnum <- paste0("B", df_visit2$patnum)

  df_visit <- bind_rows(df_visit1, df_visit2)

  # create three larger sites with only late starting patients
  df_visit3 <- sim_test_data_study(
    n_pat = 60,
    n_sites = 3,
    frac_site_with_ur = 0.4,
    ur_rate = 0.2,
    max_visit_sd = 2,
    max_visit_mean = 10
  )

  df_visit3$site_number <- paste0("C", df_visit3$site_number)

  df_visit <- bind_rows(df_visit, df_visit3)

  df_visit$study_id <- "A"

  df_site <- site_aggr(df_visit)

  study_qup8_max_visit <- df_visit %>%
    group_by(patnum) %>%
    summarise(max_visit = max(visit)) %>%
    pull(max_visit) %>%
    quantile(0.8) %>%
    round()

  testthat::expect_true(max(df_site$visit_med75) <= study_qup8_max_visit)

  # nolint start
  # plot_visit_med75(df_visit, df_site, study_id_str = "A", n_site = 6)
  # nolint end

})
