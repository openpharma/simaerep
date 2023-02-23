# test data is automatically loaded, check ./data-raw/generate_test_data.R


test_that("pat_pool() - check column names and datatypes of returnes dataframe", {

  df_pat_pool <- pat_pool(df_visit_test, df_site_test)

  expect_equal(names(df_pat_pool), c("study_id", "pat_pool"))

  expect_true(is.character(df_pat_pool$study_id))
  expect_true(is.list(df_pat_pool$pat_pool))
  expect_true(is.data.frame(df_pat_pool$pat_pool[[1]]))

})

test_that("poiss_test_site_ae_vs_study_ae() - low number of AEs at site compared to study, expect pval >0 - 0.005", {
  pval <- poiss_test_site_ae_vs_study_ae(site_ae = c(5, 3, 3, 2, 1, 6),
                                         study_ae = c(9, 8, 7, 9, 6, 7, 8),
                                         visit_med75 = 10)

  expect_true(pval < 0.005)
  expect_true(pval > 0)

})

test_that("poiss_test_site_ae_vs_study_ae() - high number of AEs at site compared to study, expect pval == 1", {
  pval <- poiss_test_site_ae_vs_study_ae(site_ae = c(9, 8, 7, 9, 6, 7, 8),
                                         study_ae = c(5, 3, 3, 2, 1, 6),
                                         visit_med75 = 10)

  expect_true(pval == 1)

})

test_that("poiss_test_site_ae_vs_study_ae() - no study AEs, single site scenario, expect pval == 1", {

  pval <- poiss_test_site_ae_vs_study_ae(site_ae = c(1),
                                         study_ae = NULL,
                                         visit_med75 = 10)

  expect_true(pval == 1)
})


test_that(
  paste("prob_lower_site_ae_vs_study_ae() - low number of AEs at site compared",
        "to study, expect prob_low >0 - 0.005"), {
  prob_low <- prob_lower_site_ae_vs_study_ae(
    site_ae = c(5, 3, 3, 2, 1, 6, 2, 1, 1, 1, 1),
    study_ae = c(9, 8, 7, 9, 6, 7, 8, 9, 9, 9)
  )

  expect_true(prob_low < 0.005)
  expect_true(prob_low > 0)

})

test_that("prob_lower_site_ae_vs_study_ae() - high number of AEs at site compared to study, expect prob_low == 1", {

  prob_low <- prob_lower_site_ae_vs_study_ae(
    site_ae = c(9, 8, 7, 9, 6, 7, 8),
    study_ae = c(5, 3, 3, 2, 1, 6)
  )

  expect_true(prob_low == 1)

})


test_that("prob_lower_site_ae_vs_study_ae() - no study AEs, single site scenario, expected prob_low == 1", {
  prob_low <- prob_lower_site_ae_vs_study_ae(
    site_ae = c(9, 8, 7, 9, 6, 7, 8),
    study_ae = NULL)

  expect_true(prob_low == 1)
})

test_that("sim_sites() - returned dataframe must not contain NA", {

  expect_true(all(complete.cases(df_sim_sites_test)))

})

test_that("sim_sites() - prob_low and pval must be between 0 - 1", {

  expect_true(all(between(df_sim_sites_test$prob_low, 0, 1)))
  expect_true(all(between(df_sim_sites_test$pval, 0, 1)))

})

test_that("sim_sites() - executing only poisson tests must be faster than using bootstrap simulations", {
  skip_on_cran() # performance on ci/cd systems is not predictable
  t_prob_low <- system.time(sim_sites(df_site_test, df_visit_test, poisson_test = FALSE, prob_lower = TRUE))
  t_ptest <- system.time(sim_sites(df_site_test, df_visit_test, prob_lower = FALSE, poisson_test = TRUE))

  expect_true(t_prob_low["elapsed"] > t_ptest["elapsed"])

})

test_that("prep_for_sim() - ae vector for site must match number of patients at site", {

  df_prep <- prep_for_sim(df_site_test, df_visit_test)

  df_prep %>%
    mutate(check = map2(
      n_pat_with_med75, n_ae_site,
      function(x, y) x == length(y))
    ) %>%
    pull(check) %>%
    unlist() %>%
    all() %>%
    expect_true()
})
