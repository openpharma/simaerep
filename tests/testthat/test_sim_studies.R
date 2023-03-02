# test data is automatically loaded, check ./data-raw/generate_test_data.R


test_that("sim_studies() - returned dataframe expect no NA and prob_low/pval 0 - 1", {

  df_sim_study <- sim_studies(
    df_site = df_site_test,
    df_visit = df_visit_test,
    r = 3,
    parallel = FALSE,
    poisson_test = TRUE,
    prob_lower = TRUE
  )

  expect_true(all(complete.cases(df_sim_study)))
  expect_true(all(between(df_sim_study$pval, 0, 1)))
  expect_true(all(between(df_sim_study$prob_low, 0, 1)))

})


test_that("sim_studies() with keep_ae = TRUE - retain individual site and study AE", {

  df_sim_study_keep <- sim_studies(
    df_site = df_site_test,
    df_visit = df_visit_test,
    r = 3,
    keep_ae = TRUE,
    parallel = FALSE,
    poisson_test = TRUE,
    prob_lower = TRUE
  )

  expect_true(all(c("n_ae_site", "n_ae_study") %in% names(df_sim_study_keep)))

})

test_that("sim_studies() - count of individual site and study AE must match other columns", {

  df_sim_study_keep <- sim_studies(
    df_site = df_site_test,
    df_visit = df_visit_test,
    r = 3,
    keep_ae = TRUE,
    parallel = FALSE,
    poisson_test = TRUE,
    prob_lower = TRUE
  )

  # check if site samples match number of patients
  expect_true(all(str_count(df_sim_study_keep$n_ae_site, ",") == df_sim_study_keep$n_pat_with_med75 - 1))

})


test_that("sim_studies() - filter studies using studies parameter", {

  df_sim_study_spec <- sim_studies(
    df_site = df_site_test,
    df_visit = df_visit_test,
    r = 3,
    keep_ae = TRUE,
    parallel = FALSE,
    poisson_test = TRUE,
    prob_lower = TRUE,
    studies = "A"
  )

  expect_true(unique(df_sim_study_spec$study_id) == "A")

})

test_that("sim_studies() - all visit_med75 values per study must have the same size of pat_pool", {

  df_sim_study_spec <- sim_studies(
    df_site = df_site_test,
    df_visit = df_visit_test,
    r = 3,
    keep_ae = TRUE,
    parallel = FALSE,
    poisson_test = TRUE,
    prob_lower = TRUE,
    studies = "A"
  )

  # check if all visit_med75 values have the same size of pat_pool
  df_check <- df_sim_study_spec %>%
    mutate(
      n_pat_with_med75_study = str_count(n_ae_study, ",") + 1,
      n_pat_pool = n_pat_with_med75_study + n_pat_with_med75
    ) %>%
    select(visit_med75, n_pat_pool) %>%
    distinct() %>%
    arrange(visit_med75)

  expect_true(nrow(df_check) == n_distinct(df_check$visit_med75))
})

test_that("sim_studies() - sampling must not repeat itself", {

  df_sim_study_spec <- sim_studies(
    df_site = df_site_test,
    df_visit = df_visit_test,
    r = 3,
    keep_ae = TRUE,
    parallel = FALSE,
    poisson_test = TRUE,
    prob_lower = TRUE,
    studies = "A"
  )

  df_check <- df_sim_study_spec %>%
    group_by(visit_med75, n_pat_with_med75, n_ae_study, r) %>%
    arrange(visit_med75, n_pat_with_med75, n_ae_study, r) %>%
    mutate(rwn = row_number())

  expect_true(max(df_check$rwn) == 1)
})

test_that("get_ecd_values() - ecd values between 0 - 1", {


  df_sim_studies <- sim_studies(
    df_site = df_site_test,
    df_visit = df_visit_test,
    r = 3,
    parallel = FALSE,
    poisson_test = TRUE,
    prob_lower = TRUE
  )

  df_prob_low <- get_ecd_values(df_sim_studies, df_sim_sites_test, "prob_low")
  df_pval <- get_ecd_values(df_sim_studies, df_sim_sites_test, "pval")

  expect_true(all(between(df_prob_low$prob_low_ecd, 0, 1)))
  expect_true(all(between(df_pval$pval_ecd, 0, 1)))

})
