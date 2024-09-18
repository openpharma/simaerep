#' @title simulate study test data
#' @description evenly distributes a number of given patients across a number of
#'   given sites. Then simulates ae development of each patient reducing the
#'   number of reported AEs for patients distributed to AE-under-reporting
#'   sites.
#' @param n_pat integer, number of patients, Default: 1000
#' @param n_sites integer, number of sites, Default: 20
#' @param frac_site_with_ur fraction of AE under-reporting sites, Default: 0
#' @param ur_rate AE under-reporting rate, will lower mean ae per visit used to
#'   simulate patients at sites flagged as AE-under-reporting. Negative Values
#'   will simulate over-reporting., Default: 0
#' @param max_visit_mean mean of the maximum number of visits of each patient,
#'   Default: 20
#' @param max_visit_sd standard deviation of maximum number of visits of each
#'   patient, Default: 4
#' @param ae_per_visit_mean mean ae per visit per patient, Default: 0.5
#' @param ae_rates vector with visit-specific ae rates, Default: Null
#' @return tibble with columns site_number, patnum, is_ur, max_visit_mean,
#'   max_visit_sd, ae_per_visit_mean, visit, n_ae
#' @details maximum visit number will be sampled from normal distribution with
#'   characteristics derived from max_visit_mean and max_visit_sd, while the ae
#'   per visit will be sampled from a poisson distribution described by
#'   ae_per_visit_mean.
#' @examples
#' set.seed(1)
#' df_visit <- sim_test_data_study(n_pat = 100, n_sites = 5)
#' df_visit[which(df_visit$patnum == "P000001"),]
#' df_visit <- sim_test_data_study(n_pat = 100, n_sites = 5,
#'     frac_site_with_ur = 0.2, ur_rate = 0.5)
#' df_visit[which(df_visit$patnum == "P000001"),]
#' ae_rates <- c(0.7, rep(0.5, 8), rep(0.3, 5))
#' sim_test_data_study(n_pat = 100, n_sites = 5, ae_rates = ae_rates)
#' @rdname sim_test_data_study
#' @export
sim_test_data_study <- function(n_pat = 1000,
                                n_sites = 20,
                                frac_site_with_ur = 0,
                                ur_rate = 0,
                                max_visit_mean = 20,
                                max_visit_sd = 4,
                                ae_per_visit_mean = 0.5,
                                ae_rates = NULL
) {

  # construct patient ae sample function
  # supports constant and non-constant ae rates

  f_sim_pat <- function(vs_max, vs_sd, is_ur) {

    if (! any(c(is.null(ae_rates), is.na(ae_rates)))) {

      if (is_ur) {
        ae_rates <- ae_rates * (1-ur_rate) # nolint
      }

      f_sample_ae <- function(max_visit) {

        # extrapolate missing ae rates by extending last rate
        fill <- rep(ae_rates[length(ae_rates)], max_visit)
        fill[seq_along(ae_rates)] <- ae_rates
        ae_rates <- fill

        aes <- integer(0)

        for (i in seq(1, max_visit)) {
          ae <- rpois(1, ae_rates[i])
          aes <- c(aes, ae)
        }

        return(aes)

      }

      ae_per_visit_mean <- mean(ae_rates)

    } else {

      if (is_ur) {
        ae_per_visit_mean <- ae_per_visit_mean * (1-ur_rate) # nolint
      }

      f_sample_ae <- function(max_visit) {

        rpois(max_visit, ae_per_visit_mean)

      }

    }

    aes <- sim_test_data_patient(
      .f_sample_max_visit = function(x) rnorm(1, mean = vs_max, sd = vs_sd),
      .f_sample_ae_per_visit = f_sample_ae
    )

    tibble(
      ae_per_visit_mean = ae_per_visit_mean,
      visit = seq(1, length(aes)),
      n_ae = aes,
    )

  }




  tibble(patnum = seq(1, n_pat)) %>%
    mutate(patnum = str_pad(patnum, width = 6, side = "left", pad = "0"),
           patnum = paste0("P", patnum),
           site_number = seq(1, n_pat),
           site_number = if (n_sites > 1) cut(.data$site_number, n_sites, labels = FALSE) else 1,
           is_ur = ifelse(.data$site_number <= (max(.data$site_number) * frac_site_with_ur), TRUE, FALSE),
           site_number = str_pad(.data$site_number, width = 4, side = "left", pad = "0"),
           site_number = paste0("S", .data$site_number),
           max_visit_mean = max_visit_mean,
           max_visit_sd = max_visit_sd,
           aes = pmap(list(.data$max_visit_mean,
                           .data$max_visit_sd,
                           .data$is_ur),
                      f_sim_pat
                      )
           ) %>%
    unnest("aes")

}

#' @title simulate patient ae reporting test data
#' @description helper function for [sim_test_data_study()][sim_test_data_study()]
#' @param .f_sample_ae_per_visit function used to sample the aes for each visit,
#'   Default: function(x) rpois(x, 0.5)
#' @param .f_sample_max_visit function used to sample the maximum number of aes,
#'   Default: function() rnorm(1, mean = 20, sd = 4)
#' @return vector containing cumulative aes
#' @details ""
#' @examples
#' replicate(5, sim_test_data_patient())
#' replicate(5, sim_test_data_patient(
#'     .f_sample_ae_per_visit = function(x) rpois(x, 1.2))
#'   )
#' replicate(5, sim_test_data_patient(
#'     .f_sample_max_visit = function() rnorm(1, mean = 5, sd = 5))
#'   )
#' @rdname sim_test_data_patient
#' @export
sim_test_data_patient <- function(.f_sample_max_visit = function() rnorm(1, mean = 20, sd = 4),
                                  .f_sample_ae_per_visit = function(max_visit) rpois(max_visit, 0.5)) {

  max_visit <- as.integer(.f_sample_max_visit())
  max_visit <- ifelse(max_visit < 1, 1, max_visit)
  aes <- .f_sample_ae_per_visit(max_visit)
  cum_aes <- cumsum(aes)

  return(cum_aes)
}

#' @title simulate single scenario
#' @description internal function called by simulate_scenarios()
#' @param n_ae_site integer vector
#' @param n_ae_study integer vector
#' @param frac_pat_with_ur double
#' @param ur_rate double
#' @return list
#' @examples
#' sim_scenario(c(5,5,5,5), c(8,8,8,8), 0.2, 0.5)
#' sim_scenario(c(5,5,5,5), c(8,8,8,8), 0.75, 0.5)
#' sim_scenario(c(5,5,5,5), c(8,8,8,8), 1, 0.5)
#' sim_scenario(c(5,5,5,5), c(8,8,8,8), 1, 1)
#' sim_scenario(c(5,5,5,5), c(8,8,8,8), 0, 0.5)
#' sim_scenario(c(5,5,5,5), c(8,8,8,8), 2, 0.5)
#' @rdname sim_scenario
#' @export
sim_scenario <- function(n_ae_site, n_ae_study, frac_pat_with_ur, ur_rate) {

  if (frac_pat_with_ur == 0 || ur_rate == 0) {
    return(list(n_ae_site = n_ae_site, n_ae_study = n_ae_study))
  }

  if (frac_pat_with_ur > 1) frac_pat_with_ur <- 1

  n_pat_site <- length(n_ae_site)
  n_pat_study <- length(n_ae_study)
  n_pat_tot <- n_pat_site + n_pat_study
  n_pat_ur <- round(n_pat_tot * frac_pat_with_ur, 0)

  max_ix_site <- min(c(n_pat_ur, n_pat_site))

  n_ae_site[1:max_ix_site] <- n_ae_site[1:max_ix_site] * (1 - ur_rate)

  if (n_pat_ur > n_pat_site) {
    max_ix_study <- n_pat_ur - n_pat_site

    n_ae_study[1:max_ix_study] <- n_ae_study[1:max_ix_study] * (1 - ur_rate)
  }

  return(list(n_ae_site = n_ae_site, n_ae_study = n_ae_study))
}

#' @title Simulate Under-Reporting Scenarios
#' @description Use with simulated portfolio data to generate under-reporting
#'   stats for specified scenarios.
#' @param df_portf dataframe as returned by \code{\link{sim_test_data_portfolio}}
#' @param extra_ur_sites numeric, set maximum number of additional
#'   under-reporting sites, see details Default: 3
#' @param ur_rate numeric vector, set under-reporting rates for scenarios
#'   Default: c(0.25, 0.5)
#' @inheritParams sim_sites
#' @param parallel logical, use parallel processing see details, Default: FALSE
#' @param progress logical, show progress bar, Default: TRUE
#' @param site_aggr_args named list of parameters passed to
#'   \code{\link{site_aggr}}, Default: list()
#' @param eval_sites_args named list of parameters passed to
#'   \code{\link{eval_sites}}, Default: list()
#' @return dataframe with the following columns:
#' \describe{
#'   \item{**study_id**}{study identification}
#'   \item{**site_number**}{site identification}
#'   \item{**n_pat**}{number of patients at site}
#'   \item{**n_pat_with_med75**}{number of patients at site with visit_med75}
#'   \item{**visit_med75**}{median(max(visit)) * 0.75}
#'   \item{**mean_ae_site_med75**}{mean AE at visit_med75 site level}
#'   \item{**mean_ae_study_med75**}{mean AE at visit_med75 study level}
#'   \item{**n_pat_with_med75_study**}{number of patients at site with
#'   visit_med75 at study excl site}
#'   \item{**extra_ur_sites**}{additional sites
#'   with under-reporting patients}
#'   \item{**frac_pat_with_ur**}{ratio of
#'   patients in study that are under-reporting}
#'   \item{**ur_rate**}{under-reporting rate}
#'   \item{**pval**}{p-value as
#'   returned by \code{\link[stats]{poisson.test}}}
#'   \item{**prob_low**}{bootstrapped probability for having mean_ae_site_med75
#'   or lower} \item{**pval_adj**}{adjusted p-values}
#'   \item{**prob_low_adj**}{adjusted bootstrapped probability for having
#'   mean_ae_site_med75 or lower} \item{**pval_prob_ur**}{probability
#'   under-reporting as 1 - pval_adj, poisson.test (use as benchmark)}
#'   \item{**prob_low_prob_ur**}{probability under-reporting as 1 -
#'   prob_low_adj, bootstrapped (use)}
#'}
#' @details The function will apply under-reporting scenarios to each site.
#'   Reducing the number of AEs by a given under-reporting (ur_rate) for all
#'   patients at the site and add the corresponding under-reporting statistics.
#'   Since the under-reporting probability is also affected by the number of
#'   other sites that are under-reporting we additionally calculate
#'   under-reporting statistics in a scenario where additional under reporting
#'   sites are present. For this we use the median number of patients per site
#'   at the study to calculate the final number of patients for which we lower
#'   the AEs in a given under-reporting scenario. We use the furrr package to
#'   implement parallel processing as these simulations can take a long time to
#'   run. For this to work we need to specify the plan for how the code should
#'   run, e.g. plan(multisession, workers = 18)
#' @examples
#' \donttest{
#' df_visit1 <- sim_test_data_study(n_pat = 100, n_sites = 10,
#'                                  frac_site_with_ur = 0.4, ur_rate = 0.6)
#'
#' df_visit1$study_id <- "A"
#'
#' df_visit2 <- sim_test_data_study(n_pat = 100, n_sites = 10,
#'                                  frac_site_with_ur = 0.2, ur_rate = 0.1)
#'
#' df_visit2$study_id <- "B"
#'
#' df_visit <- dplyr::bind_rows(df_visit1, df_visit2)
#'
#' df_site_max <- df_visit %>%
#'   dplyr::group_by(study_id, site_number, patnum) %>%
#'   dplyr::summarise(max_visit = max(visit),
#'             max_ae = max(n_ae),
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
#' df_scen <- sim_ur_scenarios(df_portf,
#'                             extra_ur_sites = 2,
#'                             ur_rate = c(0.5, 1))
#'
#'
#' df_scen
#'
#' df_perf <- get_portf_perf(df_scen)
#'
#' df_perf
#' }
#' @seealso
#'  \code{\link{sim_test_data_study}}
#'  \code{\link{get_config}}
#'  \code{\link{sim_test_data_portfolio}}
#'  \code{\link{sim_ur_scenarios}}
#'  \code{\link{get_portf_perf}}
#' @rdname sim_ur_scenarios
#' @export
sim_ur_scenarios <- function(df_portf,
                          extra_ur_sites = 3,
                          ur_rate = c(0.25, 0.5),
                          r = 1000,
                          poisson_test = FALSE,
                          prob_lower = TRUE,
                          parallel = FALSE,
                          progress = TRUE,
                          site_aggr_args = list(),
                          eval_sites_args = list(),
                          check = TRUE) {
  # checks

  stopifnot("all site_aggr_args list items must be named" = all(names(site_aggr_args) != ""))
  stopifnot("all eval_sites_args list items must be named" = all(names(eval_sites_args) != ""))
  stopifnot(is.numeric(extra_ur_sites))
  stopifnot(length(extra_ur_sites) == 1)
  extra_ur_sites <- as.integer(extra_ur_sites)

  if (progress) {
    message("aggregating site level")
  }

  if (check) {
    df_visit <- check_df_visit(df_portf)
  } else {
    df_visit <- df_portf
  }

  # get visit_med75 and number elibible patients
  df_site <- do.call(site_aggr, c(list(df_visit = df_visit), site_aggr_args))

  if (progress) {
    message("prepping for simulation")
  }

  # for every patient at site add AE count at visit_med75 to site integer vector
  # same for study
  df_sim_prep <- prep_for_sim(df_site = df_site, df_visit = df_visit)

  if (progress) {
    message("generating scenarios")
  }

  # create scenario grid

  df_mean_pat <- df_visit %>%
    group_by(.data$study_id, .data$site_number, .data$visit) %>%
    summarise(n_pat = n_distinct(.data$patnum),
              .groups = "drop") %>%
    group_by(.data$study_id, .data$visit) %>%
    summarise(mean_n_pat = mean(.data$n_pat),
              sum_n_pat = sum(.data$n_pat),
              n_sites = n_distinct(.data$site_number),
              .groups = "drop")

  # free up RAM
  remove(df_portf)
  remove(df_visit)
  gc()

  ur_rate <- ur_rate[ur_rate > 0]

  df_grid_gr0 <- df_site %>%
    select(c("study_id", "site_number", "n_pat_with_med75", "visit_med75")) %>%
    left_join(df_mean_pat, by = c(study_id = "study_id", visit_med75 = "visit")) %>%
    mutate(extra_ur_sites = list(0:extra_ur_sites)) %>%
    unnest("extra_ur_sites") %>%
    mutate(
      frac_pat_with_ur = (.data$n_pat_with_med75 + .data$extra_ur_sites * .data$mean_n_pat) /
                         .data$sum_n_pat,
      ur_rate = list(ur_rate)
    ) %>%
    unnest("ur_rate") %>%
    select(c(
      "study_id",
      "site_number",
      "extra_ur_sites",
      "frac_pat_with_ur",
      "ur_rate"
    ))

  df_grid_0 <- df_grid_gr0 %>%
    select(c("study_id", "site_number")) %>%
    distinct() %>%
    mutate(extra_ur_sites = 0,
           frac_pat_with_ur = 0,
           ur_rate = 0)

  df_grid <- bind_rows(df_grid_0, df_grid_gr0)

  # we join the parameter grid with site and study ae count vectors
  df_scen_prep <- df_sim_prep %>%
    left_join(df_grid, by = c("study_id", "site_number"))

  # generating scenarios
  # manipulate site and study ae count vectors according to scenario parameter
  df_scen <- df_scen_prep %>%
    mutate(
      scenarios = purrr::pmap(
        list(.data$n_ae_site, .data$n_ae_study, .data$frac_pat_with_ur, .data$ur_rate),
        sim_scenario
      )
    ) %>%
    select(- c("n_ae_site", "n_ae_study")) %>%
    mutate(n_ae_site = map(.data$scenarios, "n_ae_site"),
           n_ae_study = map(.data$scenarios, "n_ae_study")) %>%
    select(- "scenarios")

  if (progress) {
    message("getting under-reporting stats")
  }

  if (parallel) {
    .purrr <- furrr::future_map
    .purrr_args <- list(.options = furrr_options(seed = TRUE))
  } else {
    .purrr <- purrr::map
    .purrr_args <- list()
  }

  df_sim_sites <- df_scen %>%
    mutate(study_id_gr = .data$study_id,
           site_number_gr = .data$site_number) %>%
    group_by(.data$study_id_gr, .data$site_number_gr) %>%
    nest() %>%
    ungroup() %>%
    select(- c("study_id_gr", "site_number_gr"))


  # generate prob_low for every scenario parameter
  with_progress_cnd(
    ls_df_sim_sites <- purrr_bar(
      df_sim_sites$data,
      .purrr = .purrr,
      .f = sim_after_prep,
      .f_args = list(
        r = r,
        poisson_test = poisson_test,
        prob_lower = prob_lower,
        progress = FALSE
      ),
      .purrr_args = .purrr_args,
      .steps = nrow(df_sim_sites),
      .progress = progress
    ),
    progress = progress
  )

  if (progress) {
    message("evaluating stats")
  }

  # every row is one scenario in order to apply multiplicity correction
  # we need to join each site-level scenario back into the study context
  # with regular under-reporting score

  df_sim_sites <- bind_rows(ls_df_sim_sites)

  df_sim_sites_ur0 <- df_sim_sites %>% #nolint
    filter(.data$extra_ur_sites == 0,
           .data$frac_pat_with_ur == 0,
           .data$ur_rate == 0) %>%
    select(- c("extra_ur_sites", "frac_pat_with_ur", "ur_rate"))

  df_eval_prep <- df_sim_sites %>%
    mutate(
      study_id_gr = .data$study_id,
      site_number_gr = .data$site_number
    ) %>%
    group_by(
      .data$study_id_gr,
      .data$site_number_gr,
      .data$extra_ur_sites,
      .data$frac_pat_with_ur,
      .data$ur_rate
    ) %>%
    nest() %>%
    ungroup() %>%
    mutate(
      # add site scores to study score w/o same site
      eval = map2(
        .data$study_id_gr, .data$site_number_gr,
        ~ filter(df_sim_sites_ur0, study_id == .x, site_number != .y)
      ),
      eval = map2(
        .data$data, .data$eval,
        bind_rows
      )
    ) %>%
    select(- "data")

  # apply eval sites for every scenario
  df_eval <- df_eval_prep %>%
    mutate(
      eval = map(
        .data$eval,
        ~ do.call(eval_sites, c(list(df_sim_sites = .), eval_sites_args)),
        .progess = progress
      ),
      # filter back to site level
      eval = map2(.data$eval, .data$site_number_gr, ~ filter(.x, site_number == .y))
    ) %>%
    select(- c("study_id_gr", "site_number_gr")) %>%
    unnest(eval) %>%
    arrange(
      .data$study_id,
      .data$site_number,
      .data$extra_ur_sites,
      .data$frac_pat_with_ur,
      .data$ur_rate
    ) %>%
    select(
      c(
        "study_id",
        "site_number",
        "extra_ur_sites",
        "frac_pat_with_ur",
        "ur_rate"
      ),
      everything()
    )

  stopifnot(nrow(df_eval) == nrow(df_grid))

  return(df_eval)

}


#' @title Simulate Portfolio Test Data
#' @description Simulate visit level data from a portfolio configuration.
#' @param df_config dataframe as returned by \code{\link{get_config}}
#' @param df_ae_rates dataframe with ae rates. Default: NULL
#' @param parallel logical activate parallel processing, see details, Default: FALSE
#' @param progress logical, Default: TRUE
#'@return dataframe with the following columns: \describe{
#'  \item{**study_id**}{study identification} \item{**ae_per_visit_mean**}{mean
#'  AE per visit per study} \item{**site_number**}{site}
#'  \item{**max_visit_sd**}{standard deviation of maximum patient visits per
#'  site} \item{**max_visit_mean**}{mean of maximum patient visits per site}
#'  \item{**patnum**}{number of patients}
#'  \item{**visit**}{visit number}
#'  \item{**n_ae**}{cumulative sum of AEs}
#'}
#' @details uses \code{\link{sim_test_data_study}}.
#'   We use the `furrr` package to
#'   implement parallel processing as these simulations can take a long time to
#'   run. For this to work we need to specify the plan for how the code should
#'   run, e.g. `plan(multisession, workers = 3)
#' @examples
#' \donttest{
#' df_visit1 <- sim_test_data_study(n_pat = 100, n_sites = 10,
#'                                  frac_site_with_ur = 0.4, ur_rate = 0.6)
#'
#' df_visit1$study_id <- "A"
#'
#' df_visit2 <- sim_test_data_study(n_pat = 100, n_sites = 10,
#'                                  frac_site_with_ur = 0.2, ur_rate = 0.1)
#'
#' df_visit2$study_id <- "B"
#'
#' df_visit <- dplyr::bind_rows(df_visit1, df_visit2)
#'
#' df_site_max <- df_visit %>%
#'   dplyr::group_by(study_id, site_number, patnum) %>%
#'   dplyr::summarise(max_visit = max(visit),
#'             max_ae = max(n_ae),
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
#' df_scen <- sim_ur_scenarios(df_portf,
#'                             extra_ur_sites = 2,
#'                             ur_rate = c(0.5, 1))
#'
#'
#' df_scen
#'
#' df_perf <- get_portf_perf(df_scen)
#'
#' df_perf
#' }
#' @seealso
#'  \code{\link{sim_test_data_study}}
#'  \code{\link{get_config}}
#'  \code{\link{sim_test_data_portfolio}}
#'  \code{\link{sim_ur_scenarios}}
#'  \code{\link{get_portf_perf}}
#' @rdname sim_test_data_portfolio
#' @export
sim_test_data_portfolio <- function(df_config, df_ae_rates = NULL, parallel = FALSE, progress = TRUE) {

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
        "ae_per_visit_mean",
        "site_number",
        "max_visit_sd",
        "max_visit_mean",
        "n_pat"
      ) %in% colnames(df_config)
    )
  )

  # prep ae_rates -----------------

  if (is.null(df_ae_rates)) {
    df_config$ae_rates <- NA
  } else {
    df_ae_rates <- df_ae_rates %>%
      select(c("study_id", "ae_rate")) %>%
      group_by(.data$study_id) %>%
      nest() %>%
      mutate(
        ae_rates = map(.data$data, "ae_rate")
      ) %>%
      select(- "data")

    df_config <- df_config %>%
      inner_join(df_ae_rates, by = "study_id")
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
            .data$ae_per_visit_mean,
            .data$max_visit_sd,
            .data$max_visit_mean,
            .data$n_pat,
            .data$ae_rates
          ),
          .purrr = .purrr,
          .f = function(ae_per_visit_mean,
                        max_visit_sd,
                        max_visit_mean,
                        n_pat,
                        ae_rates) {
            sim_test_data_study(
              n_pat = n_pat,
              n_sites = 1,
              max_visit_mean = max_visit_mean,
              max_visit_sd = max_visit_sd,
              ae_per_visit_mean = ae_per_visit_mean,
              ae_rates = ae_rates
            ) %>%
              select(c(
                "patnum", "visit", "n_ae"
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
    select(- c("n_pat", "ae_rates")) %>%
    group_by(.data$study_id) %>%
    mutate(
      # patnums need to be made site exclusive
      patnum = str_pad(
        dense_rank(paste0(.data$site_number, .data$patnum)),
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
#'  patient level with max_ae and max_visit. Will filter studies with only a few
#'  sites and patients and will anonymize IDs. Portfolio configuration can be
#'  used by \code{\link{sim_test_data_portfolio}} to generate data for an
#'  artificial portfolio.
#'@param df_site dataframe aggregated on patient level with max_ae and max_visit
#'@param min_pat_per_study minimum number of patients per study, Default: 100
#'@param min_sites_per_study minimum number of sites per study, Default: 10
#'@param anonymize logical, Default: TRUE
#'@param pad_width padding width for newly created IDs, Default: 4
#'@return dataframe with the following columns: \describe{
#'  \item{**study_id**}{study identification} \item{**ae_per_visit_mean**}{mean
#'  AE per visit per study} \item{**site_number**}{site}
#'  \item{**max_visit_sd**}{standard deviation of maximum patient visits per
#'  site} \item{**max_visit_mean**}{mean of maximum patient visits per site}
#'  \item{**n_pat**}{number of patients} }
#' @examples
#' \donttest{
#' df_visit1 <- sim_test_data_study(n_pat = 100, n_sites = 10,
#'                                  frac_site_with_ur = 0.4, ur_rate = 0.6)
#'
#' df_visit1$study_id <- "A"
#'
#' df_visit2 <- sim_test_data_study(n_pat = 100, n_sites = 10,
#'                                  frac_site_with_ur = 0.2, ur_rate = 0.1)
#'
#' df_visit2$study_id <- "B"
#'
#' df_visit <- dplyr::bind_rows(df_visit1, df_visit2)
#'
#' df_site_max <- df_visit %>%
#'   dplyr::group_by(study_id, site_number, patnum) %>%
#'   dplyr::summarise(max_visit = max(visit),
#'             max_ae = max(n_ae),
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
#' df_scen <- sim_ur_scenarios(df_portf,
#'                             extra_ur_sites = 2,
#'                             ur_rate = c(0.5, 1))
#'
#'
#' df_scen
#'
#' df_perf <- get_portf_perf(df_scen)
#'
#' df_perf
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

  stopifnot(c("study_id", "site_number", "patnum", "max_visit", "max_ae") %in% colnames(df_site))
  stopifnot(nrow(df_site) == nrow(distinct(select(df_site, c("study_id", "site_number", "patnum")))))

  df_site %>%
    summarise_all(~ ! anyNA(.)) %>%
    unlist() %>%
    all() %>%
    stopifnot("NA detected" = .)

  df_site %>%
    group_by(.data$study_id, .data$patnum) %>%
    summarise(n_sites = n_distinct(.data$site_number), .groups = "drop") %>%
    mutate(check = .data$n_sites == 1) %>%
    pull(.data$check) %>%
    unlist() %>%
    all() %>%
    stopifnot("patient ids must be site exclusive" = .)

  df_config <- df_site %>%
    filter(.data$max_visit > 0) %>%
    group_by(.data$study_id) %>%
    mutate(ae_per_visit_mean = sum(.data$max_ae) / sum(.data$max_visit)) %>%
    filter(
      n_distinct(.data$patnum) >= min_pat_per_study,
      n_distinct(.data$site_number) >= min_sites_per_study
    ) %>%
    group_by(.data$study_id, .data$ae_per_visit_mean, .data$site_number) %>%
    summarise(max_visit_sd = sd(.data$max_visit),
              max_visit_mean = mean(.data$max_visit),
              n_pat = n_distinct(.data$patnum),
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
        site_number = dense_rank(.data$site_number),
        site_number = str_pad(.data$site_number, pad_width, side = "left", "0")
      ) %>%
      ungroup()
  }

  stopifnot("nrows(df_config) > 0, relax filter settings!" = nrow(df_config) > 0)

  return(df_config)
}

#' @title Get Portfolio Performance
#' @description Performance as true positive rate (tpr as tp/P) on the basis of
#'   desired false positive rates (fpr as fp/P).
#' @param df_scen dataframe as returned by \code{\link{sim_ur_scenarios}}
#' @param stat character denoting the column name of the under-reporting
#'   statistic, Default: 'prob_low_prob_ur'
#' @param fpr numeric vector specifying false positive rates, Default: c(0.001,
#'   0.01, 0.05)
#' @return dataframe
#' @details DETAILS
#' @examples
#' \donttest{
#' df_visit1 <- sim_test_data_study(n_pat = 100, n_sites = 10,
#'                                  frac_site_with_ur = 0.4, ur_rate = 0.6)
#'
#' df_visit1$study_id <- "A"
#'
#' df_visit2 <- sim_test_data_study(n_pat = 100, n_sites = 10,
#'                                  frac_site_with_ur = 0.2, ur_rate = 0.1)
#'
#' df_visit2$study_id <- "B"
#'
#' df_visit <- dplyr::bind_rows(df_visit1, df_visit2)
#'
#' df_site_max <- df_visit %>%
#'   dplyr::group_by(study_id, site_number, patnum) %>%
#'   dplyr::summarise(max_visit = max(visit),
#'                    max_ae = max(n_ae),
#'                    .groups = "drop")
#'
#' df_config <- get_config(df_site_max)
#'
#' df_config
#'
#' df_portf <- sim_test_data_portfolio(df_config)
#'
#' df_portf
#'
#' df_scen <- sim_ur_scenarios(df_portf,
#'                             extra_ur_sites = 2,
#'                             ur_rate = c(0.5, 1))
#'
#'
#' df_scen
#'
#' df_perf <- get_portf_perf(df_scen)
#'
#' df_perf
#' }
#' @seealso \code{\link{sim_test_data_study}} \code{\link{get_config}}
#' \code{\link{sim_test_data_portfolio}} \code{\link{sim_ur_scenarios}}
#' \code{\link{get_portf_perf}}
#' @rdname get_portf_perf
#' @export
get_portf_perf <- function(df_scen, stat = "prob_low_prob_ur", fpr = c(0.001, 0.01, 0.05)) {

  if (anyNA(df_scen[[stat]])) {
    mes <- df_scen %>%
      mutate(extra_ur_sites = as.factor(.data$extra_ur_sites),
             ur_rate = as.factor(.data$ur_rate)) %>%
      group_by(.data$extra_ur_sites, .data$ur_rate, .drop = FALSE) %>%
      mutate(n_sites_total = n_distinct(.data$site_number)) %>%
      group_by(.data$extra_ur_sites, .data$ur_rate, .data$n_sites_total) %>%
      filter(is.na(.data[[stat]])) %>%
      summarise(n = n_distinct(.data$site_number), .groups = "drop") %>%
      mutate(
        ratio_sites_with_na = .data$n /
                              ifelse(is.na(.data$n_sites_total),
                                     0,
                                     .data$n_sites_total)
        ) %>%
      select(c("extra_ur_sites", "ur_rate", "ratio_sites_with_na")) %>%
      knitr::kable() %>%
      paste(collapse = "\n")

    warning(
      paste("Some Simulation Scenarios have returned NA stat values.\n", mes))

  }

  stat_at_0 <- df_scen %>% # nolint
    filter(.data$ur_rate == 0, .data$frac_pat_with_ur == 0) %>%
    pull(.data[[stat]])

  df_thresh <- tibble(
      fpr = fpr
    ) %>%
    mutate(
      thresh = map_dbl(
        .data$fpr,
        ~ quantile(stat_at_0, probs =  1 - ., na.rm = TRUE)
        )
      )


  df_prep <- df_scen %>%
    mutate(data = list(df_thresh)) %>%
    unnest("data") %>%
    mutate(stat = .data[[stat]]) %>%
    group_by(.data$fpr, .data$thresh, .data$extra_ur_sites, .data$ur_rate) %>%
    summarise(
      tpr = sum(ifelse(.data$stat >= .data$thresh, 1, 0), na.rm = TRUE) /
        n_distinct(paste(.data$study_id, .data$site_number)),
      .groups = "drop")

  df_prep_0 <- df_prep %>%
    filter(.data$ur_rate == 0) %>%
    mutate(extra_ur_sites = list(unique(df_prep$extra_ur_sites))) %>%
    unnest("extra_ur_sites")

  df_prep_gr0 <- df_prep %>%
    filter(.data$ur_rate > 0)

  bind_rows(df_prep_0, df_prep_gr0) %>%
    arrange(.data$fpr, .data$ur_rate)
}
#' simulate under-reporting
#'@param df_visit, dataframe
#'@param study_id, character
#'@param site_number, character
#'@param ur_rate, double
#'@description we remove a fraction of AEs from a specific site
#'@details we determine the absolute number of AEs per patient for removal.
#'Then them remove them at the first visit.
#'We intentionally allow fractions
#'@export
#'@examples
#' df_visit <- sim_test_data_study(n_pat = 100, n_sites = 10,
#'                                  frac_site_with_ur = 0.4, ur_rate = 0.6)
#'
#' df_visit$study_id <- "A"
#'
#' df_ur <- sim_ur(df_visit, "A", site_number = "S0001", ur_rate = 0.35)
#'
#' # Example cumulated AE for first patient with 35% under-reporting
#' df_ur[df_ur$site_number == "S0001" & df_ur$patnum == "P000001",]$n_ae
#'
#' # Example cumulated AE for first patient with no under-reporting
#' df_visit[df_visit$site_number == "S0001" & df_visit$patnum == "P000001",]$n_ae
#'
sim_ur <- function(df_visit, study_id, site_number, ur_rate) {

  df_visit <- df_visit %>%
    mutate(n_ae = as.numeric(.data$n_ae))

  df_visit_study <- df_visit %>%
    filter(study_id == .env$study_id, site_number != .env$site_number)

  df_visit_site <- df_visit %>%
    filter(study_id == .env$study_id, site_number == .env$site_number)

  # determine total AE per patient
  # convert cumulative counts to single increments
  # first value needs to be AE start value
  # from start value substract ae count * ur_rate
  # convert back to cumulative count
  df_visit_site_rem <- df_visit_site %>%
    mutate(
      n_ae_pat = max(ifelse(visit == max(visit), n_ae, 0)),
      n_ae_rem = .data$n_ae - lag(.data$n_ae),
      n_ae_rem = ifelse(
        .data$visit == 1,
        .data$n_ae - (.data$n_ae_pat * .env$ur_rate),
        .data$n_ae_rem),
      n_ae = cumsum(.data$n_ae_rem),
      .by = "patnum"
    ) %>%
    mutate(
      n_ae = ifelse(n_ae < 0, 0, n_ae)
    ) %>%
    select(- "n_ae_rem", - "n_ae_pat")

  bind_rows(df_visit_study, df_visit_site_rem)

}
