

# create test data -----------------------------------------------------------
set.seed(1)
df_visit1 <- boot_sim_test_data_study(n_pat = 100, n_sites = 5,
   frac_site_with_ur = 0.4, ur_rate = 0.6)

df_visit1$study_roche <- "A"

set.seed(2)
df_visit2 <- boot_sim_test_data_study(n_pat = 100, n_sites = 5,
                                      frac_site_with_ur = 0.2, ur_rate = 0.1)

df_visit2$study_roche <- "B"

df_visit <- bind_rows(df_visit1, df_visit2)

df_site <- site_aggr(ungroup(df_visit))

df_sim_sites <- sim_sites(ungroup(df_site), ungroup(df_visit), r = 100)

df_eval <- eval_sites(ungroup(df_sim_sites), r_sim_sites = 100)


# tests -----------------------------------------------------------------------


test_that("eval_sites_with_all_NA", {

  df_na <- tibble(study_roche = "C", site_number = c("a", "b", "c")) %>%
    mutate(visit_med75 = 10,
           pval = NA,
           prob_low = NA)


  df_sim_sites <- df_sim_sites %>%
    bind_rows(df_na)

  expect_warning(df_eval <- eval_sites(df_sim_sites, r_sim_sites = 100))

  all_eval_cols_na <- df_eval %>%
    ungroup() %>%
    filter(study_roche == "C") %>%
    select(- study_roche, - site_number, - visit_med75, - n_site) %>%
    summarize_all(~ all(is.na(.))) %>%
    as.matrix() %>%
    .[1, ] %>%
    all()

  expect_true(all_eval_cols_na)

})

test_that("eval_sites", {

  df_eval <- eval_sites(df_sim_sites, r_sim_sites = 100)

  # stats should negatively correlate with P/fp ratio
  expect_true(cor(df_eval$prob_low, df_eval$prob_low_p_vs_fp_ratio) < 0)
  expect_true(cor(df_eval$pval, df_eval$pval_p_vs_fp_ratio) < 0)

})




test_that("test_pat_pool", {
  df_pat_pool <- pat_pool(df_visit, df_site)

  expect_equal(names(df_pat_pool), c("study_roche", "pat_pool"))

  expect_true(is.character(df_pat_pool$study_roche))
  expect_true(is.list(df_pat_pool$pat_pool))
  expect_true(is.data.frame(df_pat_pool$pat_pool[[1]]))
})




test_that("plot_sim_demo", {
  plot_sim_examples(size_dots = 4, size_raster_label = 10)
  plot_sim_examples(substract_ae_per_pat = c(0, 2), size_dots = 5, size_raster_label = 10)
})




test_that("plot_studies", {
  suppressWarnings({
    p <- plot_study(df_visit, df_site, df_eval, study = "BH29812")
    p <- plot_study(df_visit, df_site, df_eval, study = "BH39147")
    p <- plot_study(df_visit, df_site, df_eval, study = "BN29552")
  })

  expect_true(all(c("gg", "ggplot") %in% class(p)))
})


test_that("plot_visit_med75", {
  suppressWarnings({
    p <- plot_visit_med75(df_visit, df_site, study_roche_str = "A")
  })

  expect_true(all(c("gg", "ggplot") %in% class(p)))
})


test_that("prob_lower_site_ae_vs_study_ae", {
  prob_low <- prob_lower_site_ae_vs_study_ae(
    site_ae = c(5, 3, 3, 2, 1, 6, 2, 1, 1, 1, 1),
    study_ae = c(9, 8, 7, 9, 6, 7, 8, 9, 9, 9)
  )

  expect_true(prob_low < 0.005)
  expect_true(prob_low > 0)

  prob_low <- prob_lower_site_ae_vs_study_ae(site_ae = c(9, 8, 7, 9, 6, 7, 8), study_ae = c(5, 3, 3, 2, 1, 6))

  expect_true(prob_low == 1)

  prob_low <- prob_lower_site_ae_vs_study_ae(
    site_ae = c(5, 3, 3, 2, 1, 6, 2, 1, 1, 1, 1),
    study_ae = c(9, 8, 7, 9, 6, 7, 8, 9, 9, 9),
    parallel = T,
    r = 1e5
  )

  prob_low <- prob_lower_site_ae_vs_study_ae(site_ae = c(9, 8, 7, 9, 6, 7, 8), study_ae = NULL)

  expect_true(prob_low == 1)
})

test_that("test_sim_sites", {
  df_sim_site <- sim_sites(df_site, df_visit)

  expect_true(all(complete.cases(df_sim_site)))
  expect_true(all(between(df_sim_site$prob_low, 0, 1)))
  expect_true(all(between(df_sim_site$pval, 0, 1)))

  t_all <- system.time(sim_sites(df_site, df_visit))
  t_prob_low <- system.time(sim_sites(df_site, df_visit, poisson_test = F))
  t_ptest <- system.time(sim_sites(df_site, df_visit, prob_lower = F))

  expect_true(t_prob_low["elapsed"] > t_ptest["elapsed"])

})


test_that("test_sim_studies", {
  t_ptest <- system.time({
    df_sim_study <- sim_studies(
      df_site = df_site,
      df_visit = df_visit,
      r = 3,
      parallel = FALSE,
      poisson_test = TRUE,
      prob_lower = FALSE
    )
  })

  expect_true(all(complete.cases(df_sim_study)))
  expect_true(all(between(df_sim_study$pval, 0, 1)))

  t_prob_low <- system.time({
    df_sim_study <- sim_studies(
      df_site = df_site,
      df_visit = df_visit,
      r = 3,
      parallel = FALSE,
      poisson_test = FALSE,
      prob_lower = TRUE
    )
  })

  expect_true(all(complete.cases(df_sim_study)))
  expect_true(all(between(df_sim_study$prob_low, 0, 1)))

  expect_true(t_ptest["elapsed"] < t_prob_low["elapsed"])

  t_both <- system.time({
    df_sim_study <- sim_studies(
      df_site = df_site,
      df_visit = df_visit,
      r = 3,
      parallel = FALSE,
      poisson_test = TRUE,
      prob_lower = TRUE
    )
  })

  df_sim_study_keep <- sim_studies(
    df_site = df_site,
    df_visit = df_visit,
    r = 3,
    keep_ae = T,
    parallel = FALSE,
    poisson_test = TRUE,
    prob_lower = TRUE
  )

  expect_true(all(c("n_ae_site", "n_ae_study") %in% names(df_sim_study_keep)))

  df_sim_study_spec <- sim_studies(
    df_site = df_site,
    df_visit = df_visit,
    r = 3,
    keep_ae = TRUE,
    parallel = FALSE,
    poisson_test = TRUE,
    prob_lower = TRUE,
    studies = "A"
  )

  # check if site samples match number of patients
  expect_true(all(str_count(df_sim_study_spec$n_ae_site, ",") == df_sim_study_spec$n_pat_with_med75 - 1))

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

  # check that sampling does not repeat itself in combination
  # with seed and site configuration

  df_check <- df_sim_study_spec %>%
    group_by(visit_med75, n_pat_with_med75, n_ae_study, r) %>%
    arrange(visit_med75, n_pat_with_med75, n_ae_study, r) %>%
    mutate(rwn = row_number())

  expect_true(max(df_check$rwn) == 1)
})

test_that("site_aggr", {
  df_site <- site_aggr(df_visit)

  expect_true(all(c(
    "study_roche",
    "site_number",
    "n_patients",
    "n_pat_with_med75",
    "visit_med75",
    "mean_ae_site_med75"
  ) %in% names(df_site)))
})

test_that("ttest_site_ae_vs_study_ae", {
  pval <- ttest_site_ae_vs_study_ae(site_ae = c(5, 3, 3, 2, 1, 6), study_ae = c(9, 8, 7, 9, 6, 7, 8))

  expect_true(pval < 0.005)

  pval <- ttest_site_ae_vs_study_ae(site_ae = c(9, 8, 7, 9, 6, 7, 8), study_ae = c(5, 3, 3, 2, 1, 6))

  expect_true(pval == 1)

  pval <- ttest_site_ae_vs_study_ae(site_ae = c(1), study_ae = c(9, 8, 7, 9, 6, 7, 8))

  expect_true(pval == 1)

  pval <- ttest_site_ae_vs_study_ae(site_ae = c(1), study_ae = NULL)

  expect_true(pval == 1)
})

test_that("poiss_test_site_ae_vs_study_ae", {
  pval <- poiss_test_site_ae_vs_study_ae(site_ae = c(5, 3, 3, 2, 1, 6),
                                           study_ae = c(9, 8, 7, 9, 6, 7, 8),
                                           visit_med75 = 10)

  expect_true(pval < 0.005)

  pval <- poiss_test_site_ae_vs_study_ae(site_ae = c(9, 8, 7, 9, 6, 7, 8),
                                           study_ae = c(5, 3, 3, 2, 1, 6),
                                           visit_med75 = 10)

  expect_true(pval == 1)


  pval <- poiss_test_site_ae_vs_study_ae(site_ae = c(1),
                                           study_ae = NULL,
                                           visit_med75 = 10)

  expect_true(pval == 1)
})
