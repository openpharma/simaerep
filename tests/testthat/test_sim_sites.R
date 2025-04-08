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

test_that("poiss_test_site_ae_vs_study_ae() - invalid AEs, expect pval == 1", {

  pval <- poiss_test_site_ae_vs_study_ae(site_ae = c(Inf),
                                         study_ae = c(Inf),
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


test_that("prob_lower_site_ae_vs_study_ae() returns NA when mean_ae_site is NA", {
  expect_true(is.na(prob_lower_site_ae_vs_study_ae(site_ae = c(5, 3, 3, 2, 1, 6),
                                                   study_ae = c(NA),
                                                   r = 1000,
                                                   parallel = FALSE,
                                                   under_only = FALSE)))
})


test_that("poiss_test_site_ae_vs_study_ae() returns NA when mean_ae_site is NA", {
  expect_true(is.na(poiss_test_site_ae_vs_study_ae(site_ae = c(5, 3, 3, 2, 1, 6),
                                                   study_ae = c(NA),
                                                   visit_med75 = 10
  )))
})


test_that("warning when no patients with med75 found in study pool", {

  set.seed(1)

  df_visit1 <- sim_test_data_study(
    n_pat = 100,
    n_sites = 5,
    frac_site_with_ur = 0.4,
    ur_rate = 0.6,
    max_visit_mean = 10,
    max_visit_sd = 1
  )

  df_visit2 <- sim_test_data_study(
    n_pat = 100,
    n_sites = 1,
    frac_site_with_ur = 0,
    ur_rate = 0.6,
    max_visit_mean = 30,
    max_visit_sd = 1
    ) %>%
    mutate(
      patnum = paste(patnum, "A"),
    )

  df_visit <- dplyr::bind_rows(df_visit1, df_visit2)

  df_visit$study_id <- "A"

  df_site <- site_aggr(df_visit)

  df_prep <- prep_for_sim(df_site, df_visit)

  expect_warning(sim_after_prep(df_prep), "No adequate patients found")

})
