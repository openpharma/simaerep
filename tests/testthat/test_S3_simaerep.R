# test data is automatically loaded, check ./data-raw/generate_test_data.R

test_that("print.simaerep generic must print object description", {
  expect_snapshot(print(aerep_test))
})

test_that("is_simaerep returns TRUE", {
  expect_true(is_simaerep(aerep_test))
  expect_false(is_simaerep(LETTERS))
})

test_that("simaerep must retrieve original visit data from parent environment", {
  aerep_new <- simaerep(df_visit_test, inframe = FALSE, visit_med75 = TRUE)
  df_vs_env <- as.data.frame(aerep_new$visit)
  expect_equal(df_vs_env, df_visit_test)

  aerep_new <- simaerep(df_visit_test, inframe = TRUE, visit_med75 = TRUE)
  df_vs_env <- as.data.frame(aerep_new$visit)
  expect_equal(df_vs_env, df_visit_test)

  aerep_new <- simaerep(df_visit_test, inframe = TRUE, visit_med75 = FALSE)
  df_vs_env <- as.data.frame(aerep_new$visit)
  expect_equal(df_vs_env, df_visit_test)
})

test_that("simaerep - original dataframe unretrievable when called with slice", {
  aerep_new <- simaerep(
    df_visit_test[df_visit_test$site_number != "S0001", ],
    inframe = FALSE,
    visit_med75 = TRUE
  )

  expect_error(as.data.frame(aerep_new$visit), "Could not find original visit data")

  aerep_new <- simaerep(
    df_visit_test[df_visit_test$site_number != "S0001", ],
    inframe = TRUE,
    visit_med75 = TRUE
  )

  expect_error(as.data.frame(aerep_new$visit), "Could not find original visit data")

  aerep_new <- simaerep(
    df_visit_test[df_visit_test$site_number != "S0001", ],
    inframe = TRUE,
    visit_med75 = FALSE
  )

  expect_error(as.data.frame(aerep_new$visit), "Could not find original visit data")
})

test_that("plot.simaerep with simaerep(mult_corr = FALSE)", {
  aerep_new <- simaerep(df_visit_test, inframe = FALSE, visit_med75 = TRUE, mult_corr = FALSE)
  expect_s3_class(plot(aerep_new, what = "ur", study = "A"), "ggplot")

  aerep_new <- simaerep(df_visit_test, inframe = TRUE, visit_med75 = TRUE, mult_corr = FALSE)
  expect_s3_class(plot(aerep_new, what = "ur", study = "A"), "ggplot")

  aerep_new <- simaerep(df_visit_test, inframe = TRUE, visit_med75 = FALSE, mult_corr = FALSE)
  expect_s3_class(plot(aerep_new, what = "ur", study = "A"), "ggplot")
})

test_that("plot.simaerep with what='ur'", {
  aerep_new <- simaerep(df_visit_test, inframe = FALSE, visit_med75 = TRUE)
  expect_s3_class(plot(aerep_new, what = "ur", study = "A"), "ggplot")

  aerep_new <- simaerep(df_visit_test, inframe = TRUE, visit_med75 = TRUE)
  expect_s3_class(plot(aerep_new, what = "ur", study = "A"), "ggplot")

  aerep_new <- simaerep(df_visit_test, inframe = TRUE, visit_med75 = FALSE)
  expect_s3_class(plot(aerep_new, what = "ur", study = "A"), "ggplot")
})

test_that("plot.simaerep with what='med75'", {
  aerep_new <- simaerep(df_visit_test, inframe = FALSE, visit_med75 = TRUE)
  expect_s3_class(plot(aerep_new, what = "med75", study = "A", verbose = FALSE), "ggplot")

  aerep_new <- simaerep(df_visit_test, inframe = TRUE, visit_med75 = TRUE)
  expect_s3_class(plot(aerep_new, what = "med75", study = "A", verbose = FALSE), "ggplot")

  aerep_new <- simaerep(df_visit_test, inframe = TRUE, visit_med75 = FALSE)
  expect_s3_class(plot(aerep_new, what = "med75", study = "A", verbose = FALSE), "ggplot")
})


test_that("plot.simaerep throws error when original visit data cannot be retrieved", {
  aerep <- simaerep(df_visit_test[df_visit_test$site_number != "S0001", ])
  expect_error(plot(aerep, study = "A"))

  vs_filt <- df_visit_test[df_visit_test$site_number != "S0001", ]

  # error is mitigated if original visit data is added to plot call
  expect_s3_class(
    plot(aerep, study = "A", what = "ur", df_visit = vs_filt),
    "ggplot"
  )

  expect_s3_class(
    plot(aerep, study = "A", what = "med75", df_visit = vs_filt, verbose = FALSE),
    "ggplot"
  )
})

test_that("simaerep() with mult_corr = FALSE must not return adjusted probabilities", {

  aerep <- simaerep(df_visit_test, mult_corr = FALSE)
  expect_true(! "ae_prob_low_adj" %in% colnames(aerep$df_eval))

  aerep <- simaerep(df_visit_test, mult_corr = FALSE, under_only = FALSE)
  expect_true(! "ae_prob_low_adj" %in% colnames(aerep$df_eval))
  expect_true(! "ae_prob_high_adj" %in% colnames(aerep$df_eval))

  aerep <- simaerep(df_visit_test, mult_corr = FALSE, inframe = TRUE)
  expect_true(! "ae_prob_low_adj" %in% colnames(aerep$df_eval))

  aerep <- simaerep(df_visit_test, mult_corr = FALSE, under_only = FALSE, inframe = TRUE)
  expect_true(! "ae_prob_low_adj" %in% colnames(aerep$df_eval))
  expect_true(! "ae_prob_high_adj" %in% colnames(aerep$df_eval))

})



test_that("simaerep() produces a warning when r is not equal to param_sim_sites$r", {
  expect_warning(simaerep(df_visit_test, r = 999))
})

test_that("simaerep() produces an error when visit_med75 and inframe are FALSE", {
  expect_error(simaerep(df_visit_test, visit_med75 = FALSE, inframe = FALSE)
               , regexp = "visit_med75 parameter must be TRUE if inframe is FALSE")
})

test_that("simaerep() produces a message when the study parameter is NULL", {
  x <- simaerep(df_visit_test)
  expect_message(plot.simaerep(x), regex = "study = NULL, defaulting to study:A")
})

test_that("simaerep() produces warning messages when provided with the wrong event_name inputs", {
  expect_error(simaerep(df_visit = df_visit_events_test, event_names = "ae"),
                        regexp = "Different number of event names (1) than expected (2)", fixed = TRUE)

  df_visit_test_column <- df_visit_test |>
    rename(n_pd = n_ae)
  expect_error(simaerep(df_visit = df_visit_test_column),
               regexp = "ae not found in df_visit", fixed = TRUE)

  expect_error(simaerep(df_visit = df_visit_events_test, event_names = c("ae", "pd"), inframe = FALSE),
               regexp = "Must only have one event if inframe is FALSE", fixed = TRUE)

  df_visit_pd_test <- sim_test_data_study(
    n_pat = 100,
    n_sites = 5,
    frac_site_with_ur = 0.4,
    ur_rate = 0.6,
    event_per_visit_mean = 0.4,
    event_names = c("pd")
  )
  df_visit_pd_test$study_id <- "A"

  expect_error(simaerep(df_visit = df_visit_pd_test, event_names = "pd", inframe = FALSE),
               regexp = "Event_name must be 'ae' if inframe is FALSE", fixed = TRUE)
})


test_that("eval_sites throws the correct error when given multi-event data containing an NA", {

df_site_events_test <- site_aggr(df_visit_events_test, event_names = c("ae", "pd"))

df_sim_sites_events_test <-
  sim_sites(df_site_events_test, df_visit_events_test, r = 100, event_names = c("ae", "pd"))
df_sim_sites_events_test$ae_pval[1] <- NA


expect_warning(eval_sites(df_sim_sites_events_test, event_names = c("ae", "pd")),
               regexp = "study_id: A, site_number: S0001, a prob_low value contains NA", fixed = TRUE)})
