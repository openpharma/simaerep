test_that("simaerep() produces warning messages when provided with the wrong event_name inputs", {
  df_visit_events_test <- sim_test_data_events(event_names = c("ae", "pd"), ae_per_visit_mean = c(0.5, 0.4))


  df_visit_test_column <- df_visit_test %>%
    rename(n_pd = n_ae)
  expect_error(simaerep(df_visit = df_visit_test_column),
               regexp = "ae not found in df_visit", fixed = TRUE)

  expect_error(simaerep(df_visit = df_visit_events_test, event_names = c("ae", "pd"), inframe = FALSE),
               regexp = "Must only have one event if inframe is FALSE", fixed = TRUE)

  df_visit_pd_test <- sim_test_data_events(event_names = "pd", ae_per_visit_mean =  0.4)

  expect_error(simaerep(df_visit = df_visit_pd_test, event_names = "pd", inframe = FALSE),
               regexp = "Event_name must be 'ae' if inframe is FALSE", fixed = TRUE)
})




test_that("sim_inframe() creates appropriate column names", {
  df_visit_events_test <- sim_test_data_events(event_names = c("ae", "pd"), ae_per_visit_mean = c(0.5, 0.4))

  expect_true(all(c("pd_prob_low", "ae_prob_low") %in%
                    colnames(sim_inframe(df_visit_events_test, event_names = c("ae", "pd")))))
})


test_that("sim_test_data_study() produces the expected output for n_ae, regardless of the number of other events", {
  sim_test_data_events <- sim_test_data_study(event_names = c("ae", "pd"),
                                              ae_per_visit_mean = c(0.4, 0.4), ae_rates = list(c(0.5, 0, 1), 0.4))
  sim_test_data <- sim_test_data_study(event_names = c("ae"),
                                       ae_per_visit_mean = c(0.4), ae_rates = c(0.5, 0, 1))

  ae_rates_test_sd <- sim_test_data_events %>%
    filter(visit %in% c(1, 2)) %>%
    group_by(site_number, patnum) %>%
    summarise(stdev = sd(n_ae))
  expect_true(unique(ae_rates_test_sd[["stdev"]]) == 0
              & length(unique(ae_rates_test_sd[["stdev"]])) == 1)

  expect_equal(unique(sim_test_data_events$ae_per_visit_mean), unique(sim_test_data$ae_per_visit_mean))
})

test_that("eval_sites throws the correct error when given multi-event data containing an NA", {

  df_visit_events_test <- sim_test_data_events(event_names = c("ae", "pd"), ae_per_visit_mean = c(0.5, 0.4))

  df_site_events_test <- site_aggr(df_visit_events_test, event_names = c("ae", "pd"))


  df_sim_sites_events_test <- sim_inframe(df_site = df_site_events_test,
                                          df_visit = df_visit_events_test, r = 100, event_names = c("ae", "pd"))
  df_sim_sites_events_test$ae_prob_low[1] <- NA



  expect_warning(eval_sites(df_sim_sites_events_test, event_names = c("ae", "pd")),
                 regexp = "study_id: A, site_number: S0001, a prob_low value contains NA", fixed = TRUE)


  df_sim_sites_events_test <- sim_inframe(df_site = df_site_events_test,
                                          df_visit = df_visit_events_test, r = 100, event_names = c("ae", "pd"))
  df_sim_sites_events_test$pd_prob_low[1] <- NA


  expect_warning(eval_sites(df_sim_sites_events_test, event_names = c("ae", "pd")),
                 regexp = "study_id: A, site_number: S0001, a prob_low value contains NA", fixed = TRUE)
})



test_that("column names when using event_names as expected in df_eval", {

  events <- c("ae", "y")

  suffix <- c(
    "_prob_low",
    "_prob_low_adj",
    "_prob_low_prob_ur",
    "_prob_high",
    "_prob_high_adj",
    "_prob_high_prob_or"
  )

  cols_expected <- purrr::map(events, ~ paste0(., suffix)) %>%
    unlist()



  df_visit_events_test <- sim_test_data_events(event_names = events, ae_per_visit_mean = c(0.5, 0.4))

  df_eval_events <- simaerep(
    df_visit_events_test,
    event_names = events,
    under_only = FALSE,
    inframe = TRUE,
    mult_corr = TRUE,
    visit_med75 = FALSE
  )$df_eval

  df_eval_events_med75 <- simaerep(
    df_visit_events_test,
    event_names = events,
    under_only = FALSE,
    inframe = TRUE,
    mult_corr = TRUE,
    visit_med75 = TRUE
  )$df_eval


  df_eval_events_no_mult <- simaerep(
    df_visit_events_test,
    event_names = events,
    under_only = FALSE,
    inframe = TRUE,
    mult_corr = FALSE,
    visit_med75 = FALSE
  )$df_eval

  df_eval_events_med75_no_mult <- simaerep(
    df_visit_events_test,
    event_names = events,
    under_only = FALSE,
    inframe = TRUE,
    mult_corr = FALSE,
    visit_med75 = TRUE
  )$df_eval

  expect_true(all(cols_expected %in% colnames(df_eval_events)))
  expect_true(all(cols_expected %in% colnames(df_eval_events_med75)))

  cols_expected_no_mult <- cols_expected[! str_detect(cols_expected, "_adj")]

  expect_false(all(cols_expected %in% colnames(df_eval_events_no_mult)))
  expect_false(all(cols_expected %in% colnames(df_eval_events_med75_no_mult)))

  expect_true(all(cols_expected_no_mult %in% colnames(df_eval_events_no_mult)))
  expect_true(all(cols_expected_no_mult %in% colnames(df_eval_events_med75_no_mult)))

})


test_that("plot.simaerep works with event_names", {

  events <- c("ae", "y")

  df_visit_events_test <- sim_test_data_events(event_names = events, ae_per_visit_mean = c(0.5, 0.4))

  aerep <- simaerep(
    df_visit_events_test,
    event_names = events,
    under_only = FALSE,
    inframe = TRUE,
    mult_corr = TRUE
  )

  expect_s3_class(plot(aerep, what = "ur", study = "A",
                       df_visit = df_visit_events_test, event_names = "ae"), "ggplot")
  expect_s3_class(plot(aerep, what = "ur", study = "A",
                       df_visit = df_visit_events_test, event_names = "y"), "ggplot")
  expect_s3_class(plot(aerep, what = "ur", study = "A",
                       df_visit = df_visit_events_test, event_names = events), "ggplot")

  expect_s3_class(plot(aerep, what = "med75", study = "A",
                       df_visit = df_visit_events_test, event_names = events), "ggplot")

})



test_that("event_names works with duckdb backend", {

  events <- c("x", "y")

  suffix <- c(
    "_prob_low",
    "_prob_low_adj",
    "_prob_low_prob_ur",
    "_prob_high",
    "_prob_high_adj",
    "_prob_high_prob_or"
  )

  cols_expected <- purrr::map(events, ~ paste0(., suffix)) %>%
    unlist()

  df_visit_events_test <- sim_test_data_events(event_names = events, ae_per_visit_mean = c(0.5, 0.4))

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  df_r <- tibble(rep = seq(1, 1000))

  dplyr::copy_to(con, df_visit_events_test, "visit")
  dplyr::copy_to(con, df_r, "r")

  tbl_visit <- tbl(con, "visit")
  tbl_r <- tbl(con, "r")

  tbl_eval <- simaerep(
    tbl_visit,
    r = tbl_r,
    visit_med75 = FALSE,
    mult_corr = TRUE,
    event_names = events,
    under_only = FALSE
  )$df_eval

  tbl_eval_med75 <- simaerep(
    tbl_visit,
    r = tbl_r,
    visit_med75 = TRUE,
    mult_corr = TRUE,
    event_names = events,
    under_only = FALSE
  )$df_eval

  expect_true(all(cols_expected %in% colnames(tbl_eval)))
  expect_true(all(cols_expected %in% colnames(tbl_eval_med75)))

})
