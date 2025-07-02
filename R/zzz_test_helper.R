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

#' @title simulate test data events
#' @description generates multi-event data using sim_test_data_study()
#' @param n_pat integer, number of patients, Default: 100
#' @param n_sites integer, number of sites, Default: 5
#' @param event_per_visit_mean mean event per visit per patient, Default: 0.5
#' @param event_rates vector with visit-specific event rates, Default: Null
#' @param event_names vector, contains the event names, default = "event"
#' @return tibble with columns site_id, patient_id, is_ur, max_visit_mean,
#'   max_visit_sd, visit, and event data (events_per_visit_mean and n_events)
#' @keywords internal
sim_test_data_events <- function(
    n_pat = 100,
    n_sites = 5,
    event_per_visit_mean = 0.5,
    event_rates = c(NULL),
    event_names = list("event")) {

  set.seed(1)
  df_visit3 <- sim_test_data_study(n_pat = 100, n_sites = n_sites,
                                   frac_site_with_ur = 0.4, ur_rate = 0.6,
                                   event_per_visit_mean = event_per_visit_mean, event_names = event_names)
  df_visit3$study_id <- "A"
  set.seed(2)
  df_visit4 <- sim_test_data_study(n_pat = 100, n_sites = n_sites,
                                   frac_site_with_ur = 0.2, ur_rate = 0.1,
                                   event_per_visit_mean = event_per_visit_mean, event_names = event_names)
  df_visit4$study_id <- "B"
  df_visit_events_test <- dplyr::bind_rows(df_visit3, df_visit4)
  return(df_visit_events_test)
}
