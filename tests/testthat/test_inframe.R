# test data is automatically loaded, check ./data-raw/generate_test_data.R

test_that("simaerep_inframe and simaerep_visit_med75 must have similar results", {

  df_eval_med75 <- simaerep(df_visit_test, inframe = TRUE, visit_med75 = TRUE)$df_eval
  df_eval <- simaerep(df_visit_test, inframe = TRUE, visit_med75 = FALSE)$df_eval

  expect_equal(
    arrange(df_eval_test, study_id, site_number)$visit_med75,
    df_eval_med75$visit_med75
  )

  expect_equal(
    arrange(df_eval_test, study_id, site_number)$n_pat_with_med75,
    df_eval_med75$n_pat_with_med75
  )

  expect_equal(
    df_eval_med75 %>%
      filter(prob_low_prob_ur >= 0.95) %>%
      pull(site_number),
    df_eval_test %>%
      arrange(study_id, site_number) %>%
      filter(prob_low_prob_ur >= 0.95)  %>%
      pull(site_number)
  )

  expect_equal(
    df_eval %>%
      filter(prob_low_prob_ur >= 0.95) %>%
      pull(site_number),
    df_eval_test %>%
      arrange(study_id, site_number) %>%
      filter(prob_low_prob_ur >= 0.95)  %>%
      pull(site_number)
  )

})

test_that("simaerep_inframe must have identical counts and flags with duckdb backend", {

  df_eval_med75 <- simaerep(df_visit_test, inframe = TRUE, visit_med75 = TRUE)$df_eval
  df_eval <- simaerep(df_visit_test, inframe = TRUE, visit_med75 = FALSE)$df_eval

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  df_r <- tibble(rep = seq(1, 1000))

  dplyr::copy_to(con, df_visit_test, "visit")
  dplyr::copy_to(con, df_r, "r")

  tbl_visit <- tbl(con, "visit")
  tbl_r <- tbl(con, "r")

  tbl_eval <- simaerep(tbl_visit, r = tbl_r, visit_med75 = FALSE)$df_eval
  tbl_eval_med75 <- simaerep(tbl_visit, r = tbl_r, visit_med75 = TRUE)$df_eval

  cols_identical <- c("study_id", "site_number", "events", "visits", "events_per_visit_site")

  expect_equal(
    df_eval %>%
      select(all_of(cols_identical)),
    tbl_eval %>%
      dplyr::collect() %>%
      arrange(study_id, site_number) %>%
      select(all_of(cols_identical))
  )

  expect_equal(
    df_eval_med75 %>%
      select(all_of(cols_identical)),
    tbl_eval_med75 %>%
      dplyr::collect() %>%
      arrange(study_id, site_number) %>%
      select(all_of(cols_identical))
  )

  expect_equal(
    df_eval_med75 %>%
      filter(prob_low_prob_ur >= 0.95) %>%
      pull(site_number),
    tbl_eval_med75 %>%
      dplyr::collect() %>%
      arrange( study_id, site_number) %>%
      filter(prob_low_prob_ur >= 0.95)  %>%
      pull(site_number)
  )

  expect_equal(
    df_eval %>%
      filter(prob_low_prob_ur >= 0.95) %>%
      pull(site_number),
    tbl_eval %>%
      dplyr::collect() %>%
      arrange( study_id, site_number) %>%
      filter(prob_low_prob_ur >= 0.95)  %>%
      pull(site_number)
  )

  DBI::dbDisconnect(con)

})

test_that('p.adjust result near p_adjust_bh_inframe', {
  x <- rnorm(50, mean = c(rep(0, 500), rep(3, 500)))
  p <- 2*pnorm(sort(-abs(x)))

  df <- tibble(
      study_id = "A",
      p = p
    ) %>%
    mutate(
      pbase = p.adjust(p, method = "BH")
    ) %>%
    p_adjust_bh_inframe("p", "_simaerep")

  expect_true(all(near(df$pbase, df$p_adj, 5)))

})

test_that('p.adjust result near p_adjust_bh_inframe with duckdb', {
  x <- rnorm(50, mean = c(rep(0, 500), rep(3, 500)))
  p <- 2*pnorm(sort(-abs(x)))

  df <- tibble(
    study_id = "A",
    p = p
  ) %>%
    mutate(
      pbase = p.adjust(p, method = "BH")
    )

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  dplyr::copy_to(con, df, "df")
  tbl_df <- dplyr::tbl(con, "df")

  tbl_df <- tbl_df %>%
    p_adjust_bh_inframe("p", "_simaerep")

  df <- collect(tbl_df)

  expect_true(all(near(df$pbase, df$p_adj, 5)))

})
