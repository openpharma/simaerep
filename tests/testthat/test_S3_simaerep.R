df_visit <- get_df_visit_test()
aerep <- simaerep(df_visit)

test_that("print.simaerep generic must print object description", {
  expect_snapshot(print(aerep))
})

test_that("is_simaerep returns TRUE", {
  expect_true(is_simaerep(aerep))
  expect_false(is_simaerep(LETTERS))
})

test_that("simaerep must retrieve original visit data from parent environment", {

  df_visit <- get_df_visit_test()

  aerep_new <- simaerep(df_visit, inframe = FALSE, visit_med75 = TRUE)
  df_vs_env <- as.data.frame(aerep_new$visit)
  expect_equal(df_vs_env, df_visit)

  aerep_new <- simaerep(df_visit, inframe = TRUE, visit_med75 = TRUE)
  df_vs_env <- as.data.frame(aerep_new$visit)
  expect_equal(df_vs_env, df_visit)

  aerep_new <- simaerep(df_visit, inframe = TRUE, visit_med75 = FALSE)
  df_vs_env <- as.data.frame(aerep_new$visit)
  expect_equal(df_vs_env, df_visit)
})

test_that("simaerep - original dataframe unretrievable when called with slice", {
  aerep_new <- simaerep(
    df_visit[df_visit$site_number != "S0001", ],
    inframe = FALSE,
    visit_med75 = TRUE
  )

  expect_error(as.data.frame(aerep_new$visit), "Could not find original visit data")

  aerep_new <- simaerep(
    df_visit[df_visit$site_number != "S0001", ],
    inframe = TRUE,
    visit_med75 = TRUE
  )

  expect_error(as.data.frame(aerep_new$visit), "Could not find original visit data")

  aerep_new <- simaerep(
    df_visit[df_visit$site_number != "S0001", ],
    inframe = TRUE,
    visit_med75 = FALSE
  )

  expect_error(as.data.frame(aerep_new$visit), "Could not find original visit data")
})

test_that("plot.simaerep with simaerep(mult_corr = FALSE)", {

  df_visit <- get_df_visit_test()

  aerep_new <- simaerep(df_visit, inframe = FALSE, visit_med75 = TRUE, mult_corr = FALSE)
  expect_s3_class(plot(aerep_new, what = "ur", study = "A"), "ggplot")

  aerep_new <- simaerep(df_visit, inframe = TRUE, visit_med75 = TRUE, mult_corr = FALSE)
  expect_s3_class(plot(aerep_new, what = "ur", study = "A"), "ggplot")

  aerep_new <- simaerep(df_visit, inframe = TRUE, visit_med75 = FALSE, mult_corr = FALSE)
  expect_s3_class(plot(aerep_new, what = "ur", study = "A"), "ggplot")
})

test_that("plot.simaerep with what='ur'", {

  df_visit <- get_df_visit_test()

  aerep_new <- simaerep(df_visit, inframe = FALSE, visit_med75 = TRUE)
  expect_s3_class(plot(aerep_new, what = "ur", study = "A"), "ggplot")

  aerep_new <- simaerep(df_visit, inframe = TRUE, visit_med75 = TRUE)
  expect_s3_class(plot(aerep_new, what = "ur", study = "A"), "ggplot")

  aerep_new <- simaerep(df_visit, inframe = TRUE, visit_med75 = FALSE)
  expect_s3_class(plot(aerep_new, what = "ur", study = "A"), "ggplot")
})

test_that("plot.simaerep with what='med75'", {

  df_visit <- get_df_visit_test()

  aerep_new <- simaerep(df_visit, inframe = FALSE, visit_med75 = TRUE)
  expect_s3_class(plot(aerep_new, what = "med75", study = "A", verbose = FALSE), "ggplot")

  aerep_new <- simaerep(df_visit, inframe = TRUE, visit_med75 = TRUE)
  expect_s3_class(plot(aerep_new, what = "med75", study = "A", verbose = FALSE), "ggplot")

  aerep_new <- simaerep(df_visit, inframe = TRUE, visit_med75 = FALSE)
  expect_s3_class(plot(aerep_new, what = "med75", study = "A", verbose = FALSE), "ggplot")
})


test_that("plot.simaerep throws error when original visit data cannot be retrieved", {

  df_visit <- get_df_visit_test()

  aerep <- simaerep(df_visit[df_visit$site_number != "S0001", ])
  expect_error(plot(aerep, study = "A"))

  vs_filt <- df_visit[df_visit$site_number != "S0001", ]

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


test_that("simaerep() produces a message when the study parameter is NULL", {
  df_visit <- get_df_visit_test()
  x <- simaerep(df_visit)
  expect_message(plot.simaerep(x), regex = "study = NULL, defaulting to study:A")
})

