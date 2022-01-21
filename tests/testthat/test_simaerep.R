

# create test data -----------------------------------------------------------
set.seed(1)
df_visit1 <- sim_test_data_study(n_pat = 100, n_sites = 5,
   frac_site_with_ur = 0.4, ur_rate = 0.6)

df_visit1$study_id <- "A"

set.seed(2)
df_visit2 <- sim_test_data_study(n_pat = 100, n_sites = 5,
                                      frac_site_with_ur = 0.2, ur_rate = 0.1)

df_visit2$study_id <- "B"

df_visit <- bind_rows(df_visit1, df_visit2)

df_site <- site_aggr(df_visit)

df_sim_sites <- sim_sites(df_site, df_visit, r = 100)

df_eval <- eval_sites(df_sim_sites)


# tests -----------------------------------------------------------------------

test_that("site_aggr and sim_sites check paramater", {

  df_visit_faulty <- df_visit %>%
    group_by(study_id, site_number) %>%
    mutate(patnum = as.character(row_number())) %>%
    ungroup()

  expect_error({
      site_aggr(df_visit_faulty)
  },
  regexp = "patient ids must be site exclusive"
  )

  df_site <- site_aggr(df_visit_faulty, check = FALSE)

  expect_error({
    sim_sites(df_site, df_visit_faulty, r = 100)
    },
    regexp = "patient ids must be site exclusive"
  )

  sim_sites(df_site, df_visit_faulty, r = 100, check = FALSE)

})

test_that("check_df_visit", {
  df_visit_filt <- df_visit %>%
    filter(visit != 3)

  # nolint start
  expect_warning({df_visit_corr <- check_df_visit(df_visit_filt)})
  expect_true(3 %in% df_visit_corr$visit)
  expect_true(nrow(df_visit_corr) == nrow(df_visit))

  expect_warning({df_visit_corr <- check_df_visit(bind_rows(df_visit, df_visit))})
  expect_true(nrow(df_visit_corr) == nrow(df_visit))
  # nolint end

  expect_error({
    df_visit %>%
      mutate_at(vars(n_ae, visit), as.character) %>%
      check_df_visit()
    },
    regexp = "n_ae and vist columns must be numeric"
  )

  expect_error({
    df_visit %>%
      summarise_all(~ NA) %>%
      bind_rows(df_visit) %>%
      check_df_visit()
    },
    regexp = "NA detected in columns: study_id,site_number,patnum,n_ae,visit"
  )

  expect_error({
    df_visit %>%
      group_by(study_id, site_number) %>%
      mutate(patnum = as.character(row_number())) %>%
      ungroup() %>%
      check_df_visit()
  },
  regexp = "patient ids must be site exclusive"
  )

  expect_error({
    df_visit %>%
      group_by(study_id, site_number, patnum) %>%
      arrange(visit, .by_group = TRUE) %>%
      mutate(visit = row_number() - 1) %>%
      ungroup() %>%
      check_df_visit()
  },
  regexp = "visit numbering should start at 1"
  )

})
test_that("check_df_visit", {
  df_visit_filt <- df_visit %>%
    filter(visit != 3)

  # nolint start
  expect_warning({df_visit_corr <- check_df_visit(df_visit_filt)})
  expect_true(3 %in% df_visit_corr$visit)
  expect_true(nrow(df_visit_corr) == nrow(df_visit))

  expect_warning({df_visit_corr <- check_df_visit(bind_rows(df_visit, df_visit))})
  expect_true(nrow(df_visit_corr) == nrow(df_visit))
  # nolint end

  expect_error({
    df_visit %>%
      mutate_at(vars(n_ae, visit), as.character) %>%
      check_df_visit()
    },
    regexp = "n_ae and vist columns must be numeric"
  )

  expect_error({
    df_visit %>%
      summarise_all(~ NA) %>%
      bind_rows(df_visit) %>%
      check_df_visit()
    },
    regexp = "NA detected in columns: study_id,site_number,patnum,n_ae,visit"
  )

  expect_error({
    df_visit %>%
      group_by(study_id, site_number) %>%
      mutate(patnum = as.character(row_number())) %>%
      ungroup() %>%
      check_df_visit()
  },
  regexp = "patient ids must be site exclusive"
  )

  expect_error({
    df_visit %>%
      group_by(study_id, site_number, patnum) %>%
      arrange(visit, .by_group = TRUE) %>%
      mutate(visit = row_number() - 1) %>%
      ungroup() %>%
      check_df_visit()
  },
  regexp = "visit numbering should start at 1"
  )

})

test_that("test if returned dfs are grouped", {
  expect_false(is_grouped_df(df_site))
  expect_false(is_grouped_df(df_sim_sites))
  expect_false(is_grouped_df(df_eval))
})

test_that("eval_sites_with_all_NA", {

  df_na <- tibble(study_id = "C", site_number = c("a", "b", "c")) %>%
    mutate(visit_med75 = 10,
           pval = NA,
           prob_low = NA)


  df_sim_sites <- df_sim_sites %>%
    bind_rows(df_na)

  expect_warning(df_eval <- eval_sites(df_sim_sites, r_sim_sites = 100))

  all_eval_cols_na <- df_eval %>%
    ungroup() %>%
    filter(study_id == "C") %>%
    select(- study_id, - site_number, - visit_med75) %>%
    summarise_all(~ all(is.na(.))) %>%
    as.matrix() %>%
    .[1, ] %>%
    all()

  expect_true(all_eval_cols_na)

})


test_that("sim_test_data_study", {

  df_visit <- sim_test_data_study(
    n_pat = 3,
    n_sites = 1,
    max_visit_sd = 2,
    max_visit_mean = 40
  )

  expect_true(n_distinct(df_visit$site_number) == 1)

  df_visit <- sim_test_data_study(
    n_pat = 6,
    n_sites = 2,
    max_visit_sd = 2,
    max_visit_mean = 40
  )

  expect_true(n_distinct(df_visit$site_number) == 2)

})

test_that("eval_sites", {

  df_eval <- eval_sites(df_sim_sites)

  expect_true(
      c(
        "pval_adj",
        "pval_prob_ur",
        "prob_low_adj",
        "prob_low_prob_ur"
        ) %in% colnames(df_eval) %>%
        all()
    )

  expect_warning({
    df_eval <- eval_sites(df_sim_sites, method = NULL, r_sim_sites = 100)
  })

  # stats should negatively correlate with P/fp ratio
  expect_true(cor(df_eval$prob_low, df_eval$prob_low_p_vs_fp_ratio) < 0)
  expect_true(cor(df_eval$pval, df_eval$pval_p_vs_fp_ratio) < 0)

  # p_vs_fp_ratio must not be lower than 1

  expect_true(! any(df_eval$prob_low_p_vs_fp_ratio < 1))
  expect_true(! any(df_eval$pval_p_vs_fp_ratio < 1))
})




test_that("test_pat_pool", {
  df_pat_pool <- pat_pool(df_visit, df_site)

  expect_equal(names(df_pat_pool), c("study_id", "pat_pool"))

  expect_true(is.character(df_pat_pool$study_id))
  expect_true(is.list(df_pat_pool$pat_pool))
  expect_true(is.data.frame(df_pat_pool$pat_pool[[1]]))
})




test_that("plot_sim_demo", {
  p <- plot_sim_examples(size_dots = 4, size_raster_label = 10)
  p
  expect_true(all(c("gg", "ggplot") %in% class(p)))

  p <- plot_sim_examples(substract_ae_per_pat = c(0, 2), size_dots = 5, size_raster_label = 10)
  p
  expect_true(all(c("gg", "ggplot") %in% class(p)))
})




test_that("plot_studies", {

    p <- plot_study(df_visit, df_site, df_eval, study = "A")
    p
    p <- plot_study(df_visit, df_site, df_eval, study = "B")
    p

  expect_true(all(c("gg", "ggplot") %in% class(p)))
})


test_that("plot_visit_med75", {
  suppressWarnings({
    p <- plot_visit_med75(df_visit, df_site, study_id_str = "A")
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

  prob_low <- prob_lower_site_ae_vs_study_ae(
    site_ae = c(9, 8, 7, 9, 6, 7, 8),
    study_ae = c(5, 3, 3, 2, 1, 6)
  )

  expect_true(prob_low == 1)

  prob_low <- prob_lower_site_ae_vs_study_ae(
    site_ae = c(5, 3, 3, 2, 1, 6, 2, 1, 1, 1, 1),
    study_ae = c(9, 8, 7, 9, 6, 7, 8, 9, 9, 9),
    parallel = TRUE,
    r = 1e5
  )

  prob_low <- prob_lower_site_ae_vs_study_ae(
    site_ae = c(9, 8, 7, 9, 6, 7, 8),
    study_ae = NULL)

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
    "study_id",
    "site_number",
    "n_pat",
    "n_pat_with_med75",
    "visit_med75",
    "mean_ae_site_med75"
  ) %in% names(df_site)))
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


test_that("check_visit_med75_qup8_maximum", {
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

test_that("prep_for_sim", {

   df_prep <- prep_for_sim(df_site, df_visit)

   # ae vector for site must match number of patients at site.
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

test_that("portfolio_sim", {

   df_site_max <- df_visit %>%
     group_by(study_id, site_number, patnum) %>%
     summarise(max_visit = max(visit),
               max_ae = max(n_ae),
               .groups = "drop")

   df_config <- simaerep::get_config(
     df_site_max,
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

   df_portf <- sim_test_data_portfolio(df_config)

   expect_true(
     all(
       c("study_id", "ae_per_visit_mean", "site_number", "max_visit_sd",
         "max_visit_mean", "patnum", "visit", "n_ae") %in% colnames(df_portf)
     )
   )

   df_scen_adj <- sim_ur_scenarios(df_portf,
                             extra_ur_sites = 2,
                             ur_rate = c(0.5, 1),
                             parallel = FALSE,
                             poisson = FALSE,
                             prob_lower = TRUE,
                             progress = TRUE,
                             site_aggr_args = list(method = "med75_adj"))


   df_scen_old <- sim_ur_scenarios(df_portf,
                             extra_ur_sites = 2,
                             ur_rate = c(0.5, 1),
                             parallel = FALSE,
                             poisson = FALSE,
                             prob_lower = TRUE,
                             progress = TRUE,
                             site_aggr_args = list(method = "med75"))

   expect_false(identical(df_scen_adj, df_scen_old))

   df_perf <- get_portf_perf(df_scen_adj)

   expect_true(
     all(
       c("fpr", "thresh", "extra_ur_sites", "ur_rate",
         "tpr") %in% colnames(df_perf)
     )
   )

   # warning should be given when there are stat values with NA

   df_scen_na <- df_scen_adj %>%
     bind_rows(
       df_scen_adj %>%
         mutate(prob_low_prob_ur = NA,
                site_number = paste("A", site_number))
     )

   expect_warning(get_portf_perf(df_scen_na))
})


test_that("single site studies", {

  df_visit1 <- sim_test_data_study(
    n_pat = 100,
    n_sites = 1,
    frac_site_with_ur = 0.05,
    ur_rate = 0.4,
    ae_per_visit_mean = 0.5
  )

  df_visit1$study_id <- "A"

  df_visit2 <- sim_test_data_study(
    n_pat = 100,
    n_sites = 1,
    frac_site_with_ur = 0.05,
    ur_rate = 0.4,
    ae_per_visit_mean = 0.5
  )

  df_visit2$study_id <- "B"

  df_visit <- bind_rows(df_visit1, df_visit2)

  df_site <- site_aggr(df_visit)

  df_sim_sites <- sim_sites(df_site, df_visit, r = 1000)

  # ur stats should be NA
  df_sim_sites %>%
    filter(is.na(mean_ae_study_med75)) %>%
    select(starts_with("pval"), starts_with("prob")) %>%
    unlist() %>%
    is.na() %>%
    all() %>%
    expect_true()


  df_eval <- eval_sites(df_sim_sites)

  df_eval %>%
    filter(is.na(mean_ae_study_med75)) %>%
    select(starts_with("pval"), starts_with("prob")) %>%
    unlist() %>%
    is.na() %>%
    all() %>%
    expect_true()

})
