


df_visit <- get_df_visit_test()


df_config <- get_portf_config(
  df_visit,
  anonymize = TRUE,
  min_pat_per_study = 100,
  min_sites_per_study = 5
)

df_event_rates <- get_portf_event_rates(df_visit)

set.seed(1)

df_portf <- sim_test_data_portfolio(df_config, df_event_rates, progress = FALSE)

df_portf_static <- sim_test_data_portfolio(df_config, df_event_rates = NULL, progress = FALSE)

test_that("get_portf_event_rates() - check column names of returned data frame", {

  expect_true(
    all(
      c("study_id", "visit", "event_rate", "n_pat") %in% colnames(df_event_rates)
    )
  )
})

test_that("get_config() - check column names of returned data frame", {

  expect_true(
    all(
      c("study_id", "event_per_visit_mean", "site_id", "max_visit_sd",
        "max_visit_mean", "n_pat") %in% colnames(df_config)
    )
  )
})

test_that("sim_test_data_portfolio() - check column names of returned data frame", {
  expect_true(
    all(
      c("study_id", "event_per_visit_mean", "site_id", "max_visit_sd",
        "max_visit_mean", "patient_id", "visit", "n_event") %in% colnames(df_portf)
    )
  )

  expect_true(
    all(
      c("study_id", "event_per_visit_mean", "site_id", "max_visit_sd",
        "max_visit_mean", "patient_id", "visit", "n_event") %in% colnames(df_portf_static)
    )
  )
})


test_that("get_portf_event_rates() and get_portf_config() using in db operations", {

  skip_on_cran()

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  dplyr::copy_to(con, df_visit, "visit")
  tbl_visit <- dplyr::tbl(con, "visit")

  df_config <- get_portf_config(
    tbl_visit,
    anonymize = TRUE,
    min_pat_per_study = 100,
    min_sites_per_study = 5
  )

  df_event_rates <- get_portf_event_rates(tbl_visit)

  expect_s3_class(df_config, "data.frame")
  expect_s3_class(df_event_rates, "data.frame")

  DBI::dbDisconnect(con)

})


test_that("purrr_bar() - .slow is TRUE", {
  param <- (rep(0.25, 5))
  purr_test <- purrr_bar(param, .purrr = purrr::walk, .f = Sys.sleep, .steps = 5, .slow = TRUE)
  purr_test2 <- purrr_bar(param, .purrr = purrr::walk, .f = Sys.sleep, .steps = 5, .slow = FALSE)
  expect_equal(purr_test, purr_test2)
})
