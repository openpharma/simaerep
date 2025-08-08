

df_visit <- get_df_visit_test()


test_that("simaerep_inframe must have identical counts and flags with duckdb backend", {

  skip_on_cran()

  df_eval_med75 <- simaerep(df_visit, inframe = TRUE, visit_med75 = TRUE)$df_eval
  df_eval <- simaerep(df_visit, inframe = TRUE, visit_med75 = FALSE)$df_eval

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  df_r <- tibble(rep = seq(1, 1000))

  dplyr::copy_to(con, df_visit, "visit")
  dplyr::copy_to(con, df_r, "r")

  tbl_visit <- tbl(con, "visit")
  tbl_r <- tbl(con, "r")

  tbl_eval <- simaerep(tbl_visit, r = tbl_r, visit_med75 = FALSE)$df_eval
  tbl_eval_med75 <- simaerep(tbl_visit, r = tbl_r, visit_med75 = TRUE)$df_eval

  cols_identical <- c("study_id", "site_id", "event_count", "visits", "event_per_visit_site")

  expect_equal(
    df_eval %>%
      select(all_of(cols_identical)),
    tbl_eval %>%
      dplyr::collect() %>%
      arrange(study_id, site_id) %>%
      select(all_of(cols_identical))
  )

  expect_equal(
    df_eval_med75 %>%
      select(all_of(cols_identical)),
    tbl_eval_med75 %>%
      dplyr::collect() %>%
      arrange(study_id, site_id) %>%
      select(all_of(cols_identical))
  )

  expect_equal(
    df_eval_med75 %>%
      filter(event_prob <= - 0.95) %>%
      pull(site_id),
    tbl_eval_med75 %>%
      dplyr::collect() %>%
      arrange(study_id, site_id) %>%
      filter(event_prob <= - 0.95) %>%
      pull(site_id)
  )

  expect_equal(
    df_eval %>%
      filter(event_prob <= - 0.95) %>%
      pull(site_id),
    tbl_eval %>%
      dplyr::collect() %>%
      arrange(study_id, site_id) %>%
      filter(event_prob <= - 0.95) %>%
      pull(site_id)
  )

  DBI::dbDisconnect(con)

})

test_that("p.adjust result near p_adjust_bh_inframe", {

  set.seed(3)
  x <- rnorm(50, mean = c(rep(0, 500), rep(3, 500)))
  p <- 2 * pnorm(sort(-abs(x)))
  p <- c(1e-6, 1e-4, 1e-3, p)

  df <- tibble(
      study_id = "A",
      p = p,
      p_inframe = p
    ) %>%
    mutate(
      pbase = p.adjust(p, method = "BH")
    ) %>%
    p_adjust_bh_inframe("p_inframe") %>%
    # the implementation is only exact for low probabilities
    filter(p <= 0.05)

  expect_true(all(near(df$pbase, df$p_inframe, .0001)))

})

test_that("p.adjust result near p_adjust_bh_inframe with duckdb", {

  skip_on_cran()

  set.seed(3)
  x <- rnorm(50, mean = c(rep(0, 500), rep(3, 500)))
  p <- 2 * pnorm(sort(-abs(x)))
  p <- c(1e-6, 1e-4, 1e-3, p)

  df <- tibble(
      study_id = "A",
      p = p,
      p_inframe = p
    ) %>%
    mutate(
      pbase = p.adjust(p, method = "BH")
    )

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  dplyr::copy_to(con, df, "df")
  tbl_df <- dplyr::tbl(con, "df")

  tbl_df <- tbl_df %>%
    p_adjust_bh_inframe("p_inframe")%>%
    # the implementation is only exact for low probabilities
    filter(p <= 0.05)

  df <- collect(tbl_df)

  expect_true(all(near(df$pbase, df$p_inframe, .0001)))

})
