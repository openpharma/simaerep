# test data is automatically loaded, check ./data-raw/generate_test_data.R

test_that("check S3 test data reproducibility", {
  aerep_check <- simaerep(
    r = 100,
    df_visit_test,
    param_sim_sites = list(
      r = 100
    )
  )

  aerep_test$df_eval <- arrange(aerep_test$df_eval, study_id, site_number)

  expect_equal(aerep_check$df_sim_sites, aerep_test$df_sim_sites)
  expect_equal(aerep_check$df_eval, aerep_test$df_eval)

  visit_check <- orivisit(df_visit_test)

  expect_equal(visit_check$dim, visit_test$dim)
  expect_equal(visit_check$df_summary, visit_test$df_summary)
  expect_equal(visit_check$str_call, visit_test$str_call)

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
    r = 100,
    df_visit_check,
    param_sim_sites = list(
      poisson_test = TRUE,
      r = 100
      )
  )

  expect_equal(df_visit_test, df_visit_check)
  expect_equal(df_site_test, aerep$df_site)
  expect_equal(df_sim_sites_test, aerep$df_sim_sites)
  expect_equal(arrange(df_eval_test, study_id, site_number), aerep$df_eval)

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

  # we have fixed a mistake in sim_ur_scenarios therefore
  # the results have diverged and will not match anyomore
  # at this point we do not want to update the test data
  # to demonstrate consistency over major releases

  # expect_equal(df_scen_test, df_scen_check) #nolint
  # expect_equal(df_perf_test, df_perf_check) #nolint
})


test_that("sim_ur() ae_count as expected for max patient visits", {

  n_ae_test <- df_visit_test %>%
    filter(visit == max(visit), .by = c("study_id", "site_number", "patnum")) %>%
    filter(site_number == "S0001", study_id == "A") %>%
    pull(n_ae) %>%
    sum()

  n_ae_0p5 <- df_visit_test %>%
    sim_ur(study_id = "A", site_number = "S0001", ur_rate = 0.5) %>%
    filter(visit == max(visit), .by = c("study_id", "site_number", "patnum")) %>%
    filter(site_number == "S0001", study_id == "A") %>%
    pull(n_ae) %>%
    sum()

  expect_true(n_ae_test == n_ae_0p5 * 2)

})


test_that("sim_ur() and sim_ur_scenario() must give similar results", {

  df_sim_ur_scenarios <- sim_ur_scenarios(
      df_visit_test,
      extra_ur_sites = 0,
      ur_rate = c(0.5, 1),
      parallel = FALSE,
      poisson = FALSE,
      prob_lower = TRUE,
      progress = FALSE,
      check = FALSE,
      site_aggr_args = list(method = "med75_adj")
    ) %>%
    arrange(study_id, site_number, ur_rate)

  # sim_ur -------------------------------------

  perf <- function(df_visit, study_id, site_number, ur_rate) {
    df_vs_study <- df_visit %>%
      sim_ur(study_id, site_number, ur_rate)

    df_vs_study %>%
      simaerep(under_only = TRUE, progress = FALSE, check = FALSE) %>%
      .$df_eval %>%
      filter(.data$site_number == .env$site_number)
  }

  df_grid <- df_visit_test %>%
    distinct(study_id, site_number) %>%
    mutate(ur_rate = list(c(0, 0.5, 1))) %>%
    unnest(ur_rate)

  df_sim_ur <- df_grid %>%
    mutate(
      perf = purrr::pmap(
        list(study_id, site_number, ur_rate),
        function(x, y, z) perf(df_visit_test, x, y, z)
      )
    ) %>%
    select(- study_id, - site_number) %>%
    unnest(perf) %>%
    arrange(study_id, site_number, ur_rate)

  # compare ------------------------------------

  cols_equal <- c(
    "study_id",
    "site_number",
    "ur_rate",
    "visit_med75",
    "n_pat_with_med75",
    "mean_ae_study_med75"
  )

  expect_equal(
    df_sim_ur_scenarios[, cols_equal],
    df_sim_ur[, cols_equal]
  )

  # sim_ur calculates the number of AEs to be removed based
  # on tha latest visit of each patient and removes them from
  # the beginning. This can give smaller AE counts than removing
  # a fraction of AEs from visit_med75.

  expect_true(all(
    round(df_sim_ur$mean_ae_site_med75, 5) <=
    round(df_sim_ur_scenarios$mean_ae_site_med75, 5)
  ))

})

test_that("sim_scenario() produces a warning if the fraction of patients with underreporting is greater than 1", {
  expect_warning(sim_scenario(c(5, 5, 5, 5), c(8, 8, 8, 8), 1.2, 0.5),
                 "Fraction of patients with underreporting is greater than 1.\n  The fraction has been changed to 1")
})


test_that("sim_ur_scenarios() produces messages when progress = TRUE", {
  expect_message(sim_ur_scenarios(df_portf_test), regex = "aggregating site level")
})

test_that("sim_test_data_portfolio() produces the expected output when ae_rates are not null", {
  df_ae_rates_test <- data.frame(study_id = "0001", visit = c(1, 2, 3), ae_rate = c(0.5, 0, 1))
  ae_rates_test <- sim_test_data_portfolio(df_config_test, df_ae_rates_test)

  ae_rates_test_sd <- ae_rates_test %>%
    filter(visit %in% c(1, 2)) %>%
    group_by(study_id, patnum) %>%
    summarise(stdev = sd(n_ae))
  expect_true(unique(ae_rates_test_sd[["stdev"]]) == 0
              & length(unique(ae_rates_test_sd[["stdev"]])) == 1)

  ae_rates_test_sd <- ae_rates_test %>%
    filter(visit %in% c(2, 3)) %>%
    group_by(study_id, patnum) %>%
    summarise(stdev = sd(n_ae))

  expect_true(any(! unique(ae_rates_test_sd[["stdev"]]) == 0))
})

test_that("sim_test_data_study() alters the ae_rates value if is_ur is TRUE", {
  expect_true(unique(sim_test_data_study(frac_site_with_ur = 2,
                                         ae_rates = 2, ur_rate = 1)[["ae_per_visit_mean"]]) == 0)
})

test_that("purrr_bar() - .slow is TRUE", {
  param <- (rep(0.25, 5))
  purr_test <- purrr_bar(param, .purrr = purrr::walk, .f = Sys.sleep, .steps = 5, .slow = TRUE)
  purr_test2 <- purrr_bar(param, .purrr = purrr::walk, .f = Sys.sleep, .steps = 5, .slow = FALSE)
  expect_equal(purr_test, purr_test2)
  })

test_that("sim_test_data_study() produces an error
          when the number of event names != the number of events per visit means", {
          expect_error(sim_test_data_study(event_names = c("ae", "pd"), ae_per_visit_mean = 0.5),
regexp = "Number of events named (2) doesn't equal the number of events per visit means submitted (1)",
                       fixed = TRUE)
          })

test_that("sim_test_data_study() produces an error
          when the number of event names != the number of event rates submitted as a list", {
          expect_error(sim_test_data_study(event_names = "ae", ae_rates = list(0.3, 0.4)),
                       regexp = "Number of events named (1) doesn't equal the number of events rates submitted (2)",
                       fixed = TRUE)
})

test_that("sim_test_data_study() produces an error
          when the number of event names > 1 and event rates are submitted as a vector", {
  expect_error(sim_test_data_study(event_names = c("ae", "pd"),
                                   ae_per_visit_mean = c(0.4, 0.4), ae_rates = c(0.3, 0.4)),
               regexp = "ae_rates should be entered as a list (containing arrays) when the number of events is > 1",
               fixed = TRUE)
})
