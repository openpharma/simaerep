
df_visit <- get_df_visit_test_mapped()

test_that("check_df_visit() must fix missing visits and throw a warning", {
  df_visit_filt <- df_visit %>%
    filter(visit != 3)

  # nolint start
  expect_warning({df_visit_corr <- check_df_visit(df_visit_filt, event_names = "ae")})
  expect_true(3 %in% df_visit_corr$visit)
  expect_true(nrow(df_visit_corr) == nrow(df_visit))
  # nolint end

})

test_that("check_df_visit() must aggregate duplicated visits and throw a warning", {

  # nolint start
  expect_warning({df_visit_corr <- check_df_visit(bind_rows(df_visit, df_visit), event_names = "ae")})
  expect_true(nrow(df_visit_corr) == nrow(df_visit))
  # nolint end

})

test_that("check_df_visit() must thrown an error when n_ae and visit columns are not numeric", {
  expect_error({
    df_visit %>%
      mutate_at(vars(n_ae, visit), as.character) %>%
      check_df_visit(event_names = "ae")
  },
  regexp = "event number and visit columns must be numeric"
  )
})

test_that("check_df_visit() must thrown an error when NA's are detected", {
  expect_error({
    df_visit %>%
      summarise_all(~ NA) %>%
      bind_rows(df_visit) %>%
      check_df_visit(event_names = "ae")
  },
  regexp = "NA detected in columns: study_id, site_number, patnum, n_ae, visit"
  )
})

