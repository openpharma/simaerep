df_visit <- get_df_visit_test()
df_portf_test <- get_df_portf_test()

test_that("sim_ur() ae_count as expected for max patient visits", {

  n_ae_test <- df_visit %>%
    filter(visit == max(visit), .by = c("study_id", "site_number", "patnum")) %>%
    filter(site_number == "S0001", study_id == "A") %>%
    pull(n_ae) %>%
    sum()

  n_ae_0p5 <- df_visit %>%
    sim_ur(study_id = "A", site_number = "S0001", ur_rate = 0.5) %>%
    filter(visit == max(visit), .by = c("study_id", "site_number", "patnum")) %>%
    filter(site_number == "S0001", study_id == "A") %>%
    pull(n_ae) %>%
    sum()

  expect_true(n_ae_test == n_ae_0p5 * 2)

})


test_that("sim_ur() and sim_ur_scenario() must give similar results", {

  df_sim_ur_scenarios <- sim_ur_scenarios(
      df_visit,
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
      simaerep(
        under_only = TRUE,
        progress = FALSE,
        check = FALSE,
        inframe = FALSE,
        visit_med75 = TRUE,
        mult_corr = TRUE
      ) %>%
      .$df_eval %>%
      filter(.data$site_number == .env$site_number)
  }

  df_grid <- df_visit %>%
    distinct(study_id, site_number) %>%
    mutate(ur_rate = list(c(0, 0.5, 1))) %>%
    unnest(ur_rate)

  df_sim_ur <- df_grid %>%
    mutate(
      perf = purrr::pmap(
        list(study_id, site_number, ur_rate),
        function(x, y, z) perf(df_visit, x, y, z)
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

  df_site_max_test <- df_visit %>%
    group_by(study_id, site_number, patnum) %>%
    summarise(max_visit = max(visit),
              max_ae = max(n_ae),
              .groups = "drop")

  df_config_test <- simaerep::get_config(
    df_site_max_test,
    anonymize = TRUE,
    min_pat_per_study = 100,
    min_sites_per_study = 5
  )

  df_ae_rates_test <- data.frame(study_id = "0001", visit = c(1, 2, 3), ae_rate = c(0.5, 0, 1))
  ae_rates_test <- sim_test_data_portfolio(df_config_test, df_ae_rates_test)

  ae_rates_test_sd <- ae_rates_test %>%
    filter(visit %in% c(1, 2)) %>%
    group_by(study_id, patnum) %>%
    summarise(stdev = sd(n_ae), .groups = "drop")

  expect_true(unique(ae_rates_test_sd[["stdev"]]) == 0
              & length(unique(ae_rates_test_sd[["stdev"]])) == 1)

  ae_rates_test_sd <- ae_rates_test %>%
    filter(visit %in% c(2, 3)) %>%
    group_by(study_id, patnum) %>%
    summarise(stdev = sd(n_ae), .groups = "drop")

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
