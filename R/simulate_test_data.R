#' @title simulate study test data
#' @description evenly distributes a number of given patients across a number of
#'   given sites. Then simulates event reporting of each patient reducing the
#'   number of reported events for patients distributed to event-under-reporting
#'   sites.
#' @param n_pat integer, number of patients, Default: 1000
#' @param n_sites integer, number of sites, Default: 20
#' @param ratio_out ratio of sites with outlier, Default: 0
#' @param factor_event_rate event reporting rate factor for site outlier,
#'   will modify mean event per visit rate used for outlier sites. Negative Values
#'   will simulate under-reporting, positive values over-reporting,
#'   e.g. -0.4 -> 40% under-reporting, +0.4 -> 40% over-reporting Default: 0
#' @param max_visit_mean mean of the maximum number of visits of each patient,
#'   Default: 20
#' @param max_visit_sd standard deviation of maximum number of visits of each
#'   patient, Default: 4
#' @param event_per_visit_mean mean event per visit per patient, Default: 0.5
#' @param event_rates vector with visit-specific event rates, Default: Null
#' @param event_names vector, contains the event names, default = "event"
#' @param study_id character, Default: "A"
#' @return tibble with columns site_id, patient_id, is_out, max_visit_mean,
#'   max_visit_sd, event_per_visit_mean, visit, n_event
#' @details maximum visit number will be sampled from normal distribution with
#'   characteristics derived from max_visit_mean and max_visit_sd, while the events
#'   per visit will be sampled from a poisson distribution described by
#'   events_per_visit_mean.
#' @examples
#' set.seed(1)
#' # no outlier
#' df_visit <- sim_test_data_study(n_pat = 100, n_sites = 5)
#' df_visit[which(df_visit$patient_id == "P000001"),]
#'
#' # under-reporting outlier
#' df_visit <- sim_test_data_study(n_pat = 100, n_sites = 5,
#'     ratio_out = 0.2, factor_event_rate = -0.5)
#' df_visit[which(df_visit$patient_id == "P000001"),]
#'
#' # non-constant event rates
#' event_rates <- c(0.7, rep(0.5, 8), rep(0.3, 5))
#' sim_test_data_study(n_pat = 100, n_sites = 5, event_rates = event_rates)
#'
#' @rdname sim_test_data_study
#' @export
sim_test_data_study <- function(n_pat = 1000,
                                 n_sites = 20,
                                 ratio_out = 0,
                                 factor_event_rate = 0,
                                 max_visit_mean = 20,
                                 max_visit_sd = 4,
                                 event_per_visit_mean = c(0.5),
                                 event_rates = c(NULL),
                                 event_names = list("event"),
                                 study_id = "A"
) {

  # check ---------------------------------------------------------------------

  if (!(is.null(event_rates))) {

    if (is.numeric(event_rates) && length(event_names) > 1) {
      stop(paste0("event_rates should be entered as a list (containing arrays) when the number of events is > 1"))
    }

    if (is.list(event_rates) && length(event_rates) != length(event_names)) {
      stop(paste0("Number of events named (", length(event_names),
                                   ") doesn't equal the number of events rates submitted (", length(event_rates), ")"))
    }
  } else {

    if (length(event_names) != length(event_per_visit_mean)) {
      stop(paste0("Number of events named (", length(event_names),
                  ") doesn't equal the number of events per visit means submitted (", length(event_per_visit_mean), ")"))
    }

  }

  # construct a grid with one row per site
  tibble(patient_id = seq(1, n_pat)) %>%
    mutate(patient_id = str_pad(patient_id, width = 6, side = "left", pad = "0"),
           patient_id = paste0("P", patient_id),
           site_id = seq(1, n_pat),
           site_id = if (n_sites > 1) cut(.data$site_id, n_sites, labels = FALSE) else 1,
           is_out = ifelse(.data$site_id <= (max(.data$site_id) * ratio_out), TRUE, FALSE),
           site_id = str_pad(.data$site_id, width = 4, side = "left", pad = "0"),
           site_id = paste0("S", .data$site_id),
           max_visit_mean = max_visit_mean,
           max_visit_sd = max_visit_sd,
           # simulate patients per site
           site_patient_events = pmap(
             list(
               .data$max_visit_mean,
               .data$max_visit_sd,
               .data$is_out
             ),
             function(x, y, z) sim_pat(x, y, z, event_per_visit_mean, event_rates, event_names, factor_event_rate)
           )
    ) %>%
    unnest("site_patient_events") %>%
    mutate(
      study_id = .env$study_id
    )
}

#' simulate patients and events for sites
#' supports constant and non-constant event rates
#' @keywords internal
sim_pat <- function(vs_max,
                    vs_sd,
                    is_out,
                    event_per_visit_mean,
                    event_rates,
                    event_names,
                    factor_event_rate) {

  colnames <- c(paste0(event_names, "_per_visit_mean"), "visit",  paste0("n_", event_names))

  if (! any(c(is.null(event_rates), is.na(event_rates)))) {

    if (is_out & is.list(event_rates)) {
      event_rates <- map(event_rates, ~ . * (1 + factor_event_rate))
    }

    if (is_out & ! is.list(event_rates)) {
      event_rates <- event_rates * (1 + factor_event_rate)
    }

    f_sample_events <- function(max_visit) {

      # extrapolate missing event rates by extending last rate
      if (is.list(event_rates)) {

        fill <- map(event_rates, .f = ~(rep(.x[length(.x)], max_visit)))
        for (x in seq_along(event_rates)) {

          fill[[x]][seq_along(event_rates[[x]])] <- event_rates[[x]]
        }
      }else {

        fill <- rep(event_rates[length(event_rates)], max_visit)
        fill[seq_along(event_rates)] <- event_rates
      }

      event_rates <- fill

      for (x in seq_along(event_names)){

        events_temp <- numeric(0)

        for (i in seq(1, max_visit)) {
          event <- rpois(1, ifelse(is.list(event_rates), event_rates[[x]][i], event_rates[i]))
          events_temp <- c(events_temp, event)
        }
        if (x == 1) (events <- events_temp)
        else (events <- list(events, events_temp))
      }

      return(events)
    }

    event_per_visit_mean <- if (is.list(event_rates)) map(event_rates, mean) else (mean(event_rates))

  } else {

    if (is_out) {
      event_per_visit_mean <- event_per_visit_mean * (1 + factor_event_rate)
    }

    f_sample_events <- function(max_visit) {
      event_per_visit_mean %>%
        map(rpois, n = max_visit)
    }

  }

  events <- sim_test_data_patient(
    .f_sample_max_visit = function(x) rnorm(1, mean = vs_max, sd = vs_sd),
    .f_sample_event_per_visit = f_sample_events
  )


  sim_pat <- data.frame(event_per_visit_mean = as.list(event_per_visit_mean),
                        visit = seq(1, length(events[[1]])),
                        n_event = events)

  names(sim_pat) <- colnames
  return(sim_pat)
}


#' @title simulate patient event reporting test data
#' @description helper function for [sim_test_data_study()][sim_test_data_study()]
#' @param .f_sample_event_per_visit function used to sample the events for each visit,
#'   Default: function(x) rpois(x, 0.5)
#' @param .f_sample_max_visit function used to sample the maximum number of events,
#'   Default: function() rnorm(1, mean = 20, sd = 4)
#' @return vector containing cumulative events
#' @details ""
#' @examples
#' replicate(5, sim_test_data_patient())
#' replicate(5, sim_test_data_patient(
#'     .f_sample_event_per_visit = function(x) rpois(x, 1.2))
#'   )
#' replicate(5, sim_test_data_patient(
#'     .f_sample_max_visit = function() rnorm(1, mean = 5, sd = 5))
#'   )
#' @rdname sim_test_data_patient
#' @export
sim_test_data_patient <- function(.f_sample_max_visit = function() rnorm(1, mean = 20, sd = 4),
                                  .f_sample_event_per_visit = function(max_visit) rpois(max_visit, 0.5)) {

  max_visit <- as.integer(.f_sample_max_visit())
  max_visit <- ifelse(max_visit < 1, 1, max_visit)
  events <- .f_sample_event_per_visit(max_visit)

  if (is.list(events)) (cum_events <- events %>% map(cumsum))
  else (cum_events <- list(events) %>% map(cumsum))
  return(cum_events)
}




#' @title Simulate Portfolio Test Data
#' @description Simulate visit level data from a portfolio configuration.
#' @param df_config dataframe as returned by \code{\link{get_config}}
#' @param df_event_rates dataframe with event rates. Default: NULL
#' @param parallel logical activate parallel processing, see details, Default: FALSE
#' @param progress logical, Default: TRUE
#'@return dataframe with the following columns: \describe{
#'  \item{**study_id**}{study identification} \item{**event_per_visit_mean**}{mean
#'  event per visit per study} \item{**site_id**}{site}
#'  \item{**max_visit_sd**}{standard deviation of maximum patient visits per
#'  site} \item{**max_visit_mean**}{mean of maximum patient visits per site}
#'  \item{**patient_id**}{number of patients}
#'  \item{**visit**}{visit number}
#'  \item{**n_event**}{cumulative sum of events}
#'}
#' @details uses \code{\link{sim_test_data_study}}.
#'   We use the `furrr` package to
#'   implement parallel processing as these simulations can take a long time to
#'   run. For this to work we need to specify the plan for how the code should
#'   run, e.g. `plan(multisession, workers = 3)
#' @examples
#' \donttest{
#' df_visit1 <- sim_test_data_study(n_pat = 100, n_sites = 10,
#'                                  ratio_out = 0.4, factor_event_rate = 0.6,
#'                                  study_id = "A")#'
#'
#' df_visit2 <- sim_test_data_study(n_pat = 100, n_sites = 10,
#'                                  ratio_out = 0.2, factor_event_rate = 0.1,
#'                                  study_id = "B")
#'
#'
#' df_visit <- dplyr::bind_rows(df_visit1, df_visit2)
#'
#' df_site_max <- df_visit %>%
#'   dplyr::group_by(study_id, site_id, patient_id) %>%
#'   dplyr::summarise(max_visit = max(visit),
#'             max_event = max(n_event),
#'             .groups = "drop")
#'
#' df_config <- get_config(df_site_max)
#'
#' df_config
#'
#' df_portf <- sim_test_data_portfolio(df_config)
#'
#' df_portf
#'
#' }
#' @seealso
#'  \code{\link{sim_test_data_study}}
#'  \code{\link{get_config}}
#'  \code{\link{sim_test_data_portfolio}}
#'  \code{\link{sim_ur_scenarios}}
#'  \code{\link{get_portf_perf}}
#' @rdname sim_test_data_portfolio
#' @export
sim_test_data_portfolio <- function(df_config, df_event_rates = NULL, parallel = FALSE, progress = TRUE) {

  # checks --------------------------
  df_config <- ungroup(df_config)

  stopifnot(
    df_config %>%
      summarise_all(~ ! anyNA(.)) %>%
      unlist() %>%
      all()
  )

  stopifnot(is.data.frame(df_config))

  stopifnot(
    all(
      c("study_id",
        "event_per_visit_mean",
        "site_id",
        "max_visit_sd",
        "max_visit_mean",
        "n_pat"
      ) %in% colnames(df_config)
    )
  )

  # prep event_rates -----------------

  if (is.null(df_event_rates)) {
    df_config$event_rates <- NA
  } else {
    df_event_rates <- df_event_rates %>%
      select(c("study_id", "event_rate")) %>%
      group_by(.data$study_id) %>%
      nest() %>%
      mutate(
        event_rates = map(.data$data, "event_rate")
      ) %>%
      select(- "data")
    df_config <- df_config %>%
      inner_join(df_event_rates, by = "study_id")
  }
  # exec --------------------------

  if (parallel) {
    .purrr <- furrr::future_pmap
    .purrr_args <- list(.options = furrr_options(seed = TRUE))
  } else {
    .purrr <- purrr::pmap
    .purrr_args <- list()
  }

  with_progress_cnd(
    df_config_sim <- df_config %>%
      mutate(
        sim = purrr_bar(
          list(
            .data$event_per_visit_mean,
            .data$max_visit_sd,
            .data$max_visit_mean,
            .data$n_pat,
            .data$event_rates
          ),
          .purrr = .purrr,
          .f = function(event_per_visit_mean,
                        max_visit_sd,
                        max_visit_mean,
                        n_pat,
                        event_rates) {
            sim_test_data_study(
              n_pat = n_pat,
              n_sites = 1,
              max_visit_mean = max_visit_mean,
              max_visit_sd = max_visit_sd,
              event_per_visit_mean = event_per_visit_mean,
              event_rates = event_rates
            ) %>%
              select(c(
                "patient_id", "visit", "n_event"
              ))
          },
          .progress = progress,
          .purrr_args = .purrr_args,
          .steps = nrow(.)
        )
      ),
    progress = progress
  )



  df_portf <- df_config_sim %>%
    unnest("sim") %>%
    select(- c("n_pat", "event_rates")) %>%
    group_by(.data$study_id) %>%
    mutate(
      # patient_ids need to be made site exclusive
      patient_id = str_pad(
        dense_rank(paste0(.data$site_id, .data$patient_id)),
        width = 4,
        side = "left",
        pad = "0"
      )
    ) %>%
    ungroup()
  return(df_portf)
}

#'@title Get Portfolio Configuration
#'@description Get Portfolio configuration from a dataframe aggregated on
#'  patient level with max_event and max_visit. Will filter studies with only a few
#'  sites and patients and will anonymize IDs. Portfolio configuration can be
#'  used by \code{\link{sim_test_data_portfolio}} to generate data for an
#'  artificial portfolio.
#'@param df_site dataframe aggregated on patient level with max_event and max_visit
#'@param min_pat_per_study minimum number of patients per study, Default: 100
#'@param min_sites_per_study minimum number of sites per study, Default: 10
#'@param anonymize logical, Default: TRUE
#'@param pad_width padding width for newly created IDs, Default: 4
#'@return dataframe with the following columns: \describe{
#'  \item{**study_id**}{study identification} \item{**event_per_visit_mean**}{mean
#'  event per visit per study} \item{**site_id**}{site}
#'  \item{**max_visit_sd**}{standard deviation of maximum patient visits per
#'  site} \item{**max_visit_mean**}{mean of maximum patient visits per site}
#'  \item{**n_pat**}{number of patients} }
#' @examples
#' \donttest{
#' df_visit1 <- sim_test_data_study(n_pat = 100, n_sites = 10,
#'                                  ratio_out = 0.4, factor_event_rate = 0.6,
#'                                  study_id = "A")
#'
#'
#' df_visit2 <- sim_test_data_study(n_pat = 100, n_sites = 10,
#'                                  ratio_out = 0.2, factor_event_rate = 0.1,
#'                                  study_id = "B")
#'
#'
#' df_visit <- dplyr::bind_rows(df_visit1, df_visit2)
#'
#' df_site_max <- df_visit %>%
#'   dplyr::group_by(study_id, site_id, patient_id) %>%
#'   dplyr::summarise(max_visit = max(visit),
#'             max_event = max(n_event),
#'             .groups = "drop")
#'
#' df_config <- get_config(df_site_max)
#'
#' df_config
#'
#' df_portf <- sim_test_data_portfolio(df_config)
#'
#' df_portf
#'
#' }
#' @seealso
#'  \code{\link{sim_test_data_study}}
#'  \code{\link{get_config}}
#'  \code{\link{sim_test_data_portfolio}}
#'  \code{\link{sim_ur_scenarios}}
#'  \code{\link{get_portf_perf}}
#'@rdname get_config
#'@export
get_config <- function(df_site,
                       min_pat_per_study = 100,
                       min_sites_per_study = 10,
                       anonymize = TRUE,
                       pad_width = 4) {

  stopifnot(c("study_id", "site_id", "patient_id", "max_visit", "max_event") %in% colnames(df_site))
  stopifnot(nrow(df_site) == nrow(distinct(select(df_site, c("study_id", "site_id", "patient_id")))))

  df_site %>%
    summarise_all(~ ! anyNA(.)) %>%
    unlist() %>%
    all() %>%
    stopifnot("NA detected" = .)

  df_site %>%
    group_by(.data$study_id, .data$patient_id) %>%
    summarise(n_sites = n_distinct(.data$site_id), .groups = "drop") %>%
    mutate(check = .data$n_sites == 1) %>%
    pull(.data$check) %>%
    unlist() %>%
    all() %>%
    stopifnot("patient ids must be site exclusive" = .)

  df_config <- df_site %>%
    filter(.data$max_visit > 0) %>%
    group_by(.data$study_id) %>%
    mutate(event_per_visit_mean = sum(.data$max_event) / sum(.data$max_visit)) %>%
    filter(
      n_distinct(.data$patient_id) >= min_pat_per_study,
      n_distinct(.data$site_id) >= min_sites_per_study
    ) %>%
    group_by(.data$study_id, .data$event_per_visit_mean, .data$site_id) %>%
    summarise(max_visit_sd = sd(.data$max_visit),
              max_visit_mean = mean(.data$max_visit),
              n_pat = n_distinct(.data$patient_id),
              .groups = "drop") %>%
    mutate(max_visit_sd = ifelse(is.na(.data$max_visit_sd), 0, .data$max_visit_sd))

  if (anonymize) {
    df_config <- df_config %>%
      mutate(
        study_id = dense_rank(.data$study_id),
        study_id = str_pad(.data$study_id, pad_width, side = "left", "0")
      ) %>%
      group_by(.data$study_id) %>%
      mutate(
        site_id = dense_rank(.data$site_id),
        site_id = str_pad(.data$site_id, pad_width, side = "left", "0")
      ) %>%
      ungroup()
  }

  stopifnot("nrows(df_config) > 0, relax filter settings!" = nrow(df_config) > 0)

  return(df_config)
}


#' simulate under-reporting
#'@param df_visit, dataframe
#'@param study_id, character
#'@param site_id, character
#'@param ur_rate, double
#'@description we remove a fraction of events from a specific site
#'@details we determine the absolute number of events per patient for removal.
#'Then them remove them at the first visit.
#'We intentionally allow fractions
#'@export
#'@examples
#' df_visit <- sim_test_data_study(n_pat = 100, n_sites = 10, study_id = "A",
#'                                  ratio_out = 0.4, factor_event_rate = 0.6)
#'
#'
#' df_ur <- sim_ur(df_visit, "A", site_id = "S0001", ur_rate = 0.35)
#'
#' # Example cumulated event for first patient with 35% under-reporting
#' df_ur[df_ur$site_id == "S0001" & df_ur$patient_id == "P000001",]$n_event
#'
#' # Example cumulated event for first patient with no under-reporting
#' df_visit[df_visit$site_id == "S0001" & df_visit$patient_id == "P000001",]$n_event
#'
sim_ur <- function(df_visit, study_id, site_id, ur_rate) {

  df_visit <- df_visit %>%
    mutate(n_event = as.numeric(.data$n_event))

  df_visit_study <- df_visit %>%
    filter(study_id == .env$study_id, site_id != .env$site_id)

  df_visit_site <- df_visit %>%
    filter(study_id == .env$study_id, site_id == .env$site_id)

  # determine total event per patient
  # convert cumulative counts to single increments
  # first value needs to be event start value
  # from start value substract event count * ur_rate
  # convert back to cumulative count
  df_visit_site_rem <- df_visit_site %>%
    mutate(
      n_event_pat = max(ifelse(visit == max(visit), n_event, 0)),
      n_event_rem = .data$n_event - lag(.data$n_event),
      n_event_rem = ifelse(
        .data$visit == 1,
        .data$n_event - (.data$n_event_pat * .env$ur_rate),
        .data$n_event_rem),
      n_event = cumsum(.data$n_event_rem),
      .by = "patient_id"
    ) %>%
    mutate(
      n_event = ifelse(n_event < 0, 0, n_event)
    ) %>%
    select(- "n_event_rem", - "n_event_pat")

  bind_rows(df_visit_study, df_visit_site_rem)

}


