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
  expect_true(! "prob_low_adj" %in% colnames(aerep$df_eval))

  aerep <- simaerep(df_visit_test, mult_corr = FALSE, under_only = FALSE)
  expect_true(! "prob_low_adj" %in% colnames(aerep$df_eval))
  expect_true(! "prob_high_adj" %in% colnames(aerep$df_eval))

  aerep <- simaerep(df_visit_test, mult_corr = FALSE, inframe = TRUE)
  expect_true(! "prob_low_adj" %in% colnames(aerep$df_eval))

  aerep <- simaerep(df_visit_test, mult_corr = FALSE, under_only = FALSE, inframe = TRUE)
  expect_true(! "prob_low_adj" %in% colnames(aerep$df_eval))
  expect_true(! "prob_high_adj" %in% colnames(aerep$df_eval))

})
