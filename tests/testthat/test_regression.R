

# regression test data is saved in R/sysdata.rda
# data was generated using simaerep v0.4.2

df_visit_test # test data input
df_eval_test # test data results

df_visit <- get_df_visit_test()

test_that(paste("regression test - simaerep classic results must match",
                "saved test data results from simaerep v0.4.2"), {

  df_eval_classic <- simaerep(
    df_visit_test,
    r = 100, # test data was generated with r = 100
    inframe = FALSE,
    under_only = TRUE,
    col_names = c(
      study_id = "study_id",
      site_id = "site_number",
      patient_id = "patnum",
      visit = "visit"
    )
  )$df_eval


  df_eval_test_select <- df_eval_test %>%
    rename(
      prob = prob_low_prob_ur
    ) %>%
    mutate(
      prob = prob * -1
    ) %>%
    select(colnames(df_eval_classic)) %>%
    arrange(study_id, site_number)

  expect_equal(df_eval_classic, df_eval_test_select)

})



test_that(paste("regression test - inframe with and w/0 visit_med75 and classic",
                "algorithm must flag same sites"), {

  df_eval_inframe_med75 <- simaerep(df_visit, inframe = TRUE, visit_med75 = TRUE)$df_eval
  df_eval_inframe <- simaerep(df_visit, inframe = TRUE, visit_med75 = FALSE)$df_eval
  df_eval_classic <- simaerep(df_visit, inframe = FALSE)$df_eval


  expect_equal(
    df_eval_inframe_med75 %>%
      filter(event_prob <= - 0.95) %>%
      pull(site_id),
    df_eval_classic %>%
      filter(prob <= - 0.95)  %>%
      pull(site_id)
  )

  expect_equal(
    df_eval_inframe %>%
      filter(event_prob <= - 0.95) %>%
      pull(site_id),
    df_eval_classic %>%
      filter(prob <= - 0.95)  %>%
      pull(site_id)
  )

})

test_that(paste("regression test - inframe with visit_med75 same probabilities",
                "as classic algorithm, tolerance 0.1"), {

  df_eval_inframe_med75 <- simaerep(df_visit, inframe = TRUE, visit_med75 = TRUE)$df_eval
  df_eval_classic <- simaerep(df_visit, inframe = FALSE)$df_eval

  df_eval_inframe_med75 <- df_eval_inframe_med75 %>%
    mutate(
      mean_event_site_med75 = event_count / n_pat_with_med75
    )

  cols_equal <- c(
    "study_id",
    "site_id",
    "visit_med75",
    "n_pat_with_med75",
    "mean_event_site_med75"
  )

  expect_equal(df_eval_inframe_med75[, cols_equal], df_eval_classic[, cols_equal])

  expect_true(
    all(near(
      df_eval_inframe_med75$event_prob,
      df_eval_classic$prob,
      0.1
    ))
  )

})
