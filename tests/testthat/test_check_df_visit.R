# test data is automatically loaded, check ./data-raw/generate_test_data.R

test_that("check_df_visit() must fix missing visits and throw a warning", {
  df_visit_filt <- df_visit_test %>%
    filter(visit != 3)

  # nolint start
  expect_warning({df_visit_corr <- check_df_visit(df_visit_filt)})
  expect_true(3 %in% df_visit_corr$visit)
  expect_true(nrow(df_visit_corr) == nrow(df_visit_test))
  # nolint end

})

test_that("check_df_visit() must aggregate duplicated visits and throw a warning", {

  # nolint start
  expect_warning({df_visit_corr <- check_df_visit(bind_rows(df_visit_test, df_visit_test))})
  expect_true(nrow(df_visit_corr) == nrow(df_visit_test))
  # nolint end

})

test_that("check_df_visit() must thrown an error when n_ae and visit columns are not numeric", {
  expect_error({
    df_visit_test %>%
      mutate_at(vars(n_ae, visit), as.character) %>%
      check_df_visit()
  },
  regexp = "n_ae and visit columns must be numeric"
  )
})

test_that("check_df_visit() must thrown an error when NA's are detected", {
  expect_error({
    df_visit_test %>%
      summarise_all(~ NA) %>%
      bind_rows(df_visit_test) %>%
      check_df_visit()
  },
  regexp = "NA detected in columns: study_id,site_number,patnum,n_ae,visit"
  )
})
