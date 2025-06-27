#' Get df_visit_test mapped
#' @keywords internal
get_df_visit_test_mapped <- function() {

  df_visit <- get_df_visit_test()

  col_names <- c(
    study_id = "study_id",
    site_id = "site_id",
    patient_id = "patient_id",
    visit = "visit"
  )

  df_visit <- map_col_names(df_visit, col_names)

}


#' Get df_visit_test
#' @keywords internal
get_df_visit_test <- function() {
  set.seed(1)

  df_visit1 <- sim_test_data_study(
    n_pat = 100,
    n_sites = 5,
    ratio_out = 0.4,
    factor_event_rate = 0.6
  )

  df_visit1$study_id <- "A"

  set.seed(2)

  df_visit2 <- sim_test_data_study(
    n_pat = 100,
    n_sites = 5,
    ratio_out = 0.2,
    factor_event_rate = 0.1
  )

  df_visit2$study_id <- "B"

  df_visit_test <- dplyr::bind_rows(df_visit1, df_visit2)

  return(df_visit_test)
}

#' Get df_portf_test
#' @keywords internal
get_df_portf_test <- function() {

  col_names <- c(
    study_id = "study_id",
    site_id = "site_id",
    patient_id = "patient_id",
    visit = "visit",
    n_ae = "n_event"
  )

  df_site_max_test <- get_df_visit_test() %>%
    map_col_names(col_names) %>%
    group_by(.data$study_id, .data$site_number, .data$patnum) %>%
    summarise(max_visit = max(.data$visit),
              max_ae = max(.data$n_ae),
              .groups = "drop")

  df_config_test <- simaerep::get_config(
    df_site_max_test,
    anonymize = TRUE,
    min_pat_per_study = 100,
    min_sites_per_study = 5
  )

  set.seed(1)

  df_portf_test <- sim_test_data_portfolio(df_config_test, progress = FALSE)


}
