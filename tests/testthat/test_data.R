df_visit <- get_df_visit_test()

test_that("sim_ur() ae_count as expected for max patient visits", {

  n_ae_test <- df_visit %>%
    filter(visit == max(visit), .by = c("study_id", "site_id", "patient_id")) %>%
    filter(site_id == "S0001", study_id == "A") %>%
    pull(n_event) %>%
    sum()

  n_ae_0p5 <- df_visit %>%
    sim_ur(study_id = "A", site_id = "S0001", ur_rate = 0.5) %>%
    filter(visit == max(visit), .by = c("study_id", "site_id", "patient_id")) %>%
    filter(site_id == "S0001", study_id == "A") %>%
    pull(n_event) %>%
    sum()

  expect_true(n_ae_test == n_ae_0p5 * 2)

})


test_that("sim_test_data_portfolio() produces the expected output when ae_rates are not null", {


  df_site_max_test <- get_df_visit_test() %>%
    group_by(.data$study_id, .data$site_id, .data$patient_id) %>%
    summarise(max_visit = max(.data$visit),
              max_event = max(.data$n_event),
              .groups = "drop")

  df_config_test <- simaerep::get_config(
    df_site_max_test,
    anonymize = TRUE,
    min_pat_per_study = 100,
    min_sites_per_study = 5
  )

  df_ae_rates_test <- data.frame(study_id = "0001", visit = c(1, 2, 3), event_rate = c(0.5, 0, 1))
  ae_rates_test <- sim_test_data_portfolio(df_config_test, df_ae_rates_test)

  ae_rates_test_sd <- ae_rates_test %>%
    filter(visit %in% c(1, 2)) %>%
    group_by(study_id, patient_id) %>%
    summarise(stdev = sd(n_event), .groups = "drop")

  expect_true(unique(ae_rates_test_sd[["stdev"]]) == 0
              & length(unique(ae_rates_test_sd[["stdev"]])) == 1)

  ae_rates_test_sd <- ae_rates_test %>%
    filter(visit %in% c(2, 3)) %>%
    group_by(study_id, patient_id) %>%
    summarise(stdev = sd(n_event), .groups = "drop")

  expect_true(any(! unique(ae_rates_test_sd[["stdev"]]) == 0))
})

test_that("sim_test_data_study() alters the ae_rates value if ratio_out > 0", {

  n_unique_ae_rates <- sim_test_data_study(
      ratio_out = 0,
      event_names = "ae"
    ) %>%
    pull(.data$ae_per_visit_mean) %>%
    unique() %>%
    length()

  expect_true(n_unique_ae_rates == 1)

  n_unique_ae_rates <- sim_test_data_study(
      ratio_out = 0.3,
      factor_event_rate = -0.5,
      event_names = "ae"
    ) %>%
    pull(.data$ae_per_visit_mean) %>%
    unique() %>%
    length()

  expect_true(n_unique_ae_rates == 2)

})

test_that("purrr_bar() - .slow is TRUE", {

  param <- (rep(0.25, 5))
  purr_test <- purrr_bar(param, .purrr = purrr::walk, .f = Sys.sleep, .steps = 5, .slow = TRUE)
  purr_test2 <- purrr_bar(param, .purrr = purrr::walk, .f = Sys.sleep, .steps = 5, .slow = FALSE)
  expect_equal(purr_test, purr_test2)

})

test_that(paste("sim_test_data_study() produces an error",
          "when the number of event names != the number of events per visit means"), {

  expect_error(
    sim_test_data_study(event_names = c("ae", "pd"), event_per_visit_mean = 0.5),
    regexp = "Number of events named (2) doesn't equal the number of events per visit means submitted (1)",
    fixed = TRUE
  )

})

test_that(paste("sim_test_data_study() non constant event rates for two event names"), {

  event_rates_ae <- c(0.7, rep(0.5, 8), rep(0.3, 5))
  event_rates_pd <- c(0.3, rep(0.4, 8), rep(0.1, 5))

  df_visit <- sim_test_data_study(
   n_pat = 100,
   n_sites = 5,
   ratio_out = 0.25,
   event_names = c("ae", "pd"),
   event_rates = list(event_rates_ae, event_rates_pd)
  )

  expect_true(all(c("n_ae", "n_pd") %in% colnames(df_visit)))

})

test_that(paste("sim_test_data_study() produces an error",
          "when the number of event names != the number of event rates submitted as a list"), {

  expect_error(
    sim_test_data_study(event_names = "ae", event_rates = list(0.3, 0.4)),
    regexp = "Number of events named (1) doesn't equal the number of events rates submitted (2)",
    fixed = TRUE
  )

})

test_that(paste("sim_test_data_study() produces an error",
          "when the number of event names > 1 and event rates are submitted as a vector"), {

  expect_error(
    sim_test_data_study(
      event_names = c("ae", "pd"),
      event_per_visit_mean = c(0.4, 0.4),
      event_rates = c(0.3, 0.4)
    ),
    regexp = "event_rates should be entered as a list (containing arrays) when the number of events is > 1",
    fixed = TRUE
  )

})
