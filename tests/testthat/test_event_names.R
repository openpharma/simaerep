
df_visit <- get_df_visit_test()



test_that(paste("sim_test_data_study() produces the expected output for n_ae",
          ", regardless of the number of other events"), {

  sim_test_data_events <- sim_test_data_study(
    event_names = c("ae", "pd"),
    event_per_visit_mean = c(0.4, 0.4),
    event_rates = list(c(0.5, 0, 1), 0.4)
  )

  sim_test_data <- sim_test_data_study(
    event_names = c("ae"),
    event_per_visit_mean = c(0.4),
    event_rates = c(0.5, 0, 1)
  )

  ae_rates_test_sd <- sim_test_data_events %>%
    filter(visit %in% c(1, 2)) %>%
    group_by(site_id, patient_id) %>%
    summarise(stdev = sd(n_ae), .groups = "drop")

  expect_true(
    unique(ae_rates_test_sd[["stdev"]]) == 0 &
    length(unique(ae_rates_test_sd[["stdev"]])) == 1
  )

  expect_equal(
    unique(sim_test_data_events$ae_per_visit_mean),
    unique(sim_test_data$ae_per_visit_mean)
  )
})

test_that("eval_sites throws the correct warning when given multi-event data containing an NA", {

  df_visit_events_test <- sim_test_data_events(event_names = c("ae", "pd"), event_per_visit_mean = c(0.5, 0.4)) %>%
    rename(
      site_number = site_id,
      patnum = patient_id
    )

  df_site_events_test <- site_aggr(df_visit_events_test, event_names = c("ae", "pd"))


  df_sim_sites_events_test <- sim_inframe(df_site = df_site_events_test,
                                          df_visit = df_visit_events_test, r = 100, event_names = c("ae", "pd"))

  df_sim_sites_events_test$ae_prob_low[1] <- NA


  expect_warning(eval_sites(df_sim_sites_events_test, event_names = c("ae", "pd"), under_only = FALSE),
                 regexp = "study_id: A, site_number: S0001, a prob_low value contains NA", fixed = TRUE)


  df_sim_sites_events_test <- sim_inframe(df_site = df_site_events_test,
                                          df_visit = df_visit_events_test, r = 100, event_names = c("ae", "pd"))

  df_sim_sites_events_test$pd_prob_low[1] <- NA


  expect_warning(eval_sites(df_sim_sites_events_test, event_names = c("ae", "pd"), under_only = FALSE),
                 regexp = "study_id: A, site_number: S0001, a prob_low value contains NA", fixed = TRUE)
})



test_that("column names when using event_names as expected in df_eval", {

  events <- c("ae", "y")

  cols_expected_inframe <- purrr::map(events, ~ paste0(., c("_prob", "_delta"))) %>%
    unlist()

  cols_expected <- paste0(events, "_prob")

  df_visit_events_test <- sim_test_data_events(event_names = events, event_per_visit_mean = c(0.5, 0.4))

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


  expect_true(all(cols_expected_inframe %in% colnames(df_eval_events)))
  expect_true(all(cols_expected %in% colnames(df_eval_events_med75)))
  expect_false(all(cols_expected_inframe %in% colnames(df_eval_events_med75)))

})

test_that("S3 orivisits works with event_names", {

  events <- c("ae", "y")

  df_visit_events_test <- sim_test_data_events(event_names = events, event_per_visit_mean = c(0.5, 0.4))

  aerep <- simaerep(
    df_visit_events_test,
    event_names = events,
    under_only = FALSE,
    inframe = TRUE,
    mult_corr = TRUE
  )

  expect_s3_class(as.data.frame(aerep$visit), "data.frame")

})


test_that("plot.simaerep works with event_names", {

  events <- c("ae", "y")

  df_visit_events_test <- sim_test_data_events(event_names = events, event_per_visit_mean = c(0.5, 0.4))

  aerep <- simaerep(
    df_visit = df_visit_events_test,
    event_names = events,
    under_only = FALSE,
    inframe = TRUE,
    mult_corr = TRUE
  )

  expect_s3_class(plot(aerep, what = "prob", study = "A", plot_event = "ae"), "ggplot")
  expect_s3_class(plot(aerep, what = "prob", study = "A", plot_event = "y"), "ggplot")
  expect_s3_class(plot(aerep, what = "prob", study = "A", plot_event = events), "ggplot")

  expect_s3_class(
    plot(aerep, what = "med75", study = "A", plot_event = events, verbose = FALSE),
    "ggplot"
  )

})

test_that("get_cum_mean_event_dev works with non-default event_names", {

  events <- c("x", "y")

  df_visit_events_test <- sim_test_data_events(event_names = events, event_per_visit_mean = c(0.5, 0.4))  %>%
    rename(
      site_number = site_id,
      patnum = patient_id
    )

  df_cum_mean_event_dev <- get_cum_mean_event_dev(df_visit_events_test, event_names = events)
  expect_true(all(glue("cum_mean_dev_{events}") %in% colnames(df_cum_mean_event_dev)))

})

test_that("event_names works with duckdb backend", {

  skip_on_cran()

  events <- c("x", "y")

  cols_expected_inframe <- purrr::map(events, ~ paste0(., c("_prob", "_delta"))) %>%
    unlist()

  cols_expected <- paste0(events, "_prob")

  df_visit_events_test <- sim_test_data_events(event_names = events, event_per_visit_mean = c(0.5, 0.4))

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

  expect_true(all(cols_expected_inframe %in% colnames(tbl_eval)))
  expect_true(all(cols_expected %in% colnames(tbl_eval_med75)))
  expect_false(all(cols_expected_inframe %in% colnames(tbl_eval_med75)))

})
