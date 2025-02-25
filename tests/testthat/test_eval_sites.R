# test data is automatically loaded, check ./data-raw/generate_test_data.R


test_that("eval_sites() must not return a grouped dataframe", {
  expect_false(is_grouped_df(df_eval_test))
})

test_that(
  paste("eval_sites() must throw warning if stats returned by sim_sites()",
        "include NA and subsequently maintain NA values"), {

  df_na <- tibble(study_id = "C", site_number = c("a", "b", "c")) %>%
    mutate(visit_med75 = 10,
           pval = NA,
           prob_low = NA)


  df_sim_sites <- df_sim_sites_test %>%
    bind_rows(df_na)

  # a new line is created for each site with NA values
  # this cannot be captured with expect_warning()
  # https://testthat.r-lib.org/articles/third-edition.html
  # as of testthat 3.0
  expect_snapshot(
    df_eval <- eval_sites(df_sim_sites, r_sim_sites = 100)
  )

  all_eval_cols_na <- df_eval %>%
    ungroup() %>%
    filter(study_id == "C") %>%
    select(- study_id, - site_number, - visit_med75) %>%
    summarise_all(~ all(is.na(.))) %>%
    as.matrix() %>%
    .[1, ] %>%
    all()

  expect_true(all_eval_cols_na)

})


test_that("eval_sites() - check column names of returned data frame", {

  expect_true(
    c(
      "pval_adj",
      "pval_prob_ur",
      "prob_low_adj",
      "prob_low_prob_ur"
    ) %in% colnames(df_eval_test) %>%
      all()
  )
})
