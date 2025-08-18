
#' @title Integrity check for df_visit.
#' @description Internal function used by all functions that accept df_visit as a parameter.
#'   Checks for NA columns, numeric visits and AEs, implicitly missing and
#'   duplicated visits.
#' @inheritParams site_aggr
#' @return corrected df_visit
#' @examples
#'
#' df_visit <- sim_test_data_study(
#'   n_pat = 100,
#'   n_sites = 5,
#'   ratio_out = 0.4,
#'   factor_event_rate = 0.6
#'   ) %>%
#'   # internal functions require internal column names
#'   dplyr::rename(
#'     site_number = site_id,
#'     patnum = patient_id
#'   )
#'
#' df_visit_filt <- df_visit %>%
#'   dplyr::filter(visit != 3)
#'
#' df_visit_corr <- check_df_visit(df_visit_filt)
#' 3 %in% df_visit_corr$visit
#' nrow(df_visit_corr) == nrow(df_visit)
#'
#' df_visit_corr <- check_df_visit(dplyr::bind_rows(df_visit, df_visit))
#' nrow(df_visit_corr) == nrow(df_visit)
#'
#' @rdname check_df_visit
#' @keywords internal
#' @seealso [simaerep][simaerep()]
#' @export
check_df_visit <- function(df_visit, event_names = c("event")) {

  df_visit <- ungroup(df_visit)
  colnames <- paste0("n_", event_names)
  stopifnot(
    all(c("study_id", "site_number", "patnum", colnames, "visit") %in% colnames(df_visit))
  )

  no_na_cols <- c(
    "study_id",
    "site_number",
    "patnum",
    colnames,
    "visit"
  )

  cols_na <- get_any_na(df_visit, no_na_cols) %>%
    unlist()

  if (any(cols_na)) {
    stop(paste("NA detected in columns:", paste(no_na_cols[cols_na], collapse = ", ")))
  }

  df_visit %>%
    group_by(.data$study_id, .data$patnum) %>%
    summarise(n_sites = n_distinct(.data$site_number), .groups = "drop") %>%
    mutate(check = .data$n_sites == 1) %>%
    pull(.data$check) %>%
    unlist() %>%
    all() %>%
    stopifnot("patient ids must be site exclusive" = .)

  df_visit %>%
    mutate(check = .data$visit > 0) %>%
    pull(.data$check) %>%
    unlist() %>%
    all() %>%
    stopifnot("visit numbering should start at 1" = .)

  if (inherits(df_visit, "data.frame")) {
    df_visit %>%
      summarise_at(
        vars(c(
          all_of(colnames),
          "visit"
        )),
        ~ is.numeric(.)
      ) %>%
      unlist() %>%
      all() %>%
      stopifnot("event number and visit columns must be numeric" = .)

    df_visit <- exp_implicit_missing_visits(df_visit, event_names)
    df_visit <- aggr_duplicated_visits(df_visit, event_names)

  }

  return(df_visit)
}

#' @title Aggregate duplicated visits.
#' @description Internal function called by [check_df_visit()][check_df_visit].
#' @inheritParams site_aggr
#' @return df_visit corrected
#' @rdname aggr_duplicated_visits
#' @keywords internal
aggr_duplicated_visits <- function(df_visit, event_names = "ae") {
  colnames <- paste0("n_", event_names)
  df_visit_out <- df_visit %>%
    group_by(
      .data$study_id,
      .data$site_number,
      .data$patnum,
      .data$visit
    ) %>%
    summarise(across(.cols = all_of(colnames), .fns = ~max(.x), .names = "{colnames}"), .groups = "drop")

  if (nrow(df_visit_out) < nrow(df_visit)) {
    warning("Duplicated visit entries for some patients detected and corrected.")
  }

  return(df_visit_out)
}

#' @title Expose implicitly missing visits.
#' @description Internal function called by [check_df_visit()][check_df_visit].
#' @inheritParams site_aggr
#' @return df_visit corrected
#' @rdname exp_implicit_missing_visits
#' @keywords internal
exp_implicit_missing_visits <- function(df_visit, event_names = "ae") {

  colnames <- paste0("n_", event_names)
  df_complete <- df_visit %>%
    group_by(.data$study_id) %>%
    mutate(min_study_visit = min(.data$visit),
              max_study_visit = max(.data$visit)) %>%
    select(c(
      "study_id",
      "site_number",
      "patnum",
      "min_study_visit",
      "max_study_visit"
    )) %>%
    distinct() %>%
    mutate(
      visit = map2(
        .data$min_study_visit,
        .data$max_study_visit,
        function(x, y) seq(x, y, 1)
      )
    ) %>%
    unnest("visit") %>%
    select(c(
      "study_id",
      "site_number",
      "patnum",
      "min_study_visit",
      "visit"
    ))

  df_visit_out <- df_visit %>%
    group_by(.data$study_id, .data$site_number, .data$patnum) %>%
    mutate(min_visit_pat = min(.data$visit),
           max_visit_pat = max(.data$visit)) %>%
    ungroup() %>%
    right_join(
      df_complete,
      by = c("study_id", "site_number", "patnum", "visit")
    ) %>%
    group_by(.data$study_id, .data$site_number, .data$patnum) %>%
    arrange(.data$visit) %>%
    fill(all_of(colnames), .direction = "down") %>%
    mutate(
      min_visit_pat = min(.data$min_visit_pat, na.rm = TRUE),
      max_visit_pat = max(.data$max_visit_pat, na.rm = TRUE)
    ) %>%
    group_by(.data$study_id) %>%
    mutate(min_study_visit = min(.data$min_study_visit, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(
      .data$visit >= .data$min_study_visit &
      .data$visit <= .data$max_visit_pat
    ) %>%
    mutate(across(.cols = all_of(colnames), .fns = ~(ifelse(is.na(.x), 0, .x)), .names = "{colnames}")) %>%
    select(c(
      "study_id",
      "site_number",
      "patnum",
      all_of(colnames),
      "visit"
    )) %>%
    arrange(.data$study_id, .data$site_number, .data$patnum, .data$visit)

  if (nrow(df_visit_out) > nrow(df_visit)) {
    warning("implicitly missing visit numbers detected and corrected")
  }

  stopifnot(nrow(df_visit_out) >= nrow(df_visit))

  return(df_visit_out)
}

#' @title Aggregate visit to patient level.
#' @description Internal function used by [site_aggr()][site_aggr()] and
#' [plot_visit_med75()][plot_visit_med75()], adds the maximum visit for each patient.
#' @param df_visit dataframe
#' @return dataframe
#' @rdname pat_aggr
#' @keywords internal
pat_aggr <- function(df_visit) {

  df_pat <- df_visit %>%
    group_by(.data$study_id, .data$site_number, .data$patnum) %>%
    summarise(max_visit_per_pat = max(.data$visit),
              .groups = "drop")

  return(df_pat)

}

#' @title Get site mean ae development.
#' @description Internal function used by [site_aggr()][site_aggr()],
#' returns mean AE development from visit 0 to visit_med75.
#' @param df_visit dataframe
#' @param df_pat dataframe as returned by pat_aggr()
#' @param df_site dataframe as returned by site_aggr()
#' @param event_names vector, contains the event names, default = "ae"
#' @return dataframe
#' @rdname get_site_mean_ae_dev
#' @keywords internal
get_site_mean_ae_dev <- function(df_visit, df_pat, df_site, event_names = c("ae")) {
  colnames <- paste0("mean_", event_names, "_site") #nolint
  colsearch <- paste0("n_", event_names)

  df_visit %>%
    left_join(df_pat, by = c("study_id", "site_number", "patnum")) %>%
    left_join(df_site, by = c("study_id", "site_number")) %>%
    filter(
      .data$visit <= .data$visit_med75,
      .data$max_visit_per_pat >= .data$visit_med75
    ) %>%
    group_by(.data$study_id, .data$site_number, .data$visit_med75, .data$visit) %>%
    summarise(across(all_of(colsearch), mean, .names = "{colnames}"),
              .groups = "drop")

}
#' @title Get visit_med75.
#' @description Internal function used by [site_aggr()][site_aggr()].
#' @param df_pat dataframe as returned by [pat_aggr()][pat_aggr()]
#' @inheritParams site_aggr
#' @return dataframe
#' @rdname get_visit_med75
#' @keywords internal
get_visit_med75 <- function(df_pat,
                            method = "med75_adj",
                            min_pat_pool = 0.2) {

  if (inherits(df_pat, "data.frame")) {
    fun_arrange <- arrange
  } else {
    fun_arrange <- window_order
  }

  # we have to separate the aggregation steps into two steps
  # we need to reference visit_med75 for counting patients with
  # visit_med75 not all db backends may support this

  df_visit_med75 <- df_pat %>%
    summarise(
      visit_med75 = ceiling(median(.data$max_visit_per_pat) * 0.75),
      .by = c("study_id", "site_number")
    )

  df_site <- df_pat %>%
    left_join(
      df_visit_med75,
      by = c("study_id", "site_number")
    ) %>%
    summarise(
      n_pat = n_distinct(.data$patnum),
      n_pat_with_med75 = sum(ifelse(
        .data$max_visit_per_pat >= .data$visit_med75,
        1, 0)
      ),
      .by = c("study_id", "site_number", "visit_med75")
    )


  if (method == "med75_adj") {

    df_site <- df_site %>%
      right_join(
        df_pat,
        by = c("study_id", "site_number")
      ) %>%
      mutate(
        pat_has_visit_med75 = ifelse(.data$max_visit_per_pat >= .data$visit_med75, 1, 0)
      )

    if (inherits(df_pat, "data.frame")) {
      # this cannot be exactly replicated, we maintain this method so that results
      # do not change compared to previous versions
      df_qup8 <- df_site %>%
        summarise(
          study_qup8_max_visit = quantile(.data$max_visit_per_pat, probs = c(1 - min_pat_pool)),
          study_qup8_max_visit = round(.data$study_qup8_max_visit, 0),
          .by = "study_id"
        ) %>%
        select(c("study_id", "study_qup8_max_visit"))

    } else {
      # calculate max_visit that covers at least min_map_pool
      # wer are not using quantile for db backend compatibility
      df_qup8 <- df_site %>%
        mutate(
          rnk = percent_rank(.data$max_visit_per_pat),
          .by = "study_id"
        ) %>%
        filter(.data$rnk <= 1 - .env$min_pat_pool) %>%
        fun_arrange(.data$study_id, desc(.data$max_visit_per_pat)) %>%
        filter(row_number() == 1, .by = "study_id") %>%
        select(c("study_id", study_qup8_max_visit = "max_visit_per_pat"))
    }

    df_site <- df_site %>%
      left_join(
        df_qup8,
        by = c("study_id")
      ) %>%
      summarise(
        # from all patients that have a visit_med75 we take the smallest maximum
        visit_med75 = min(
          ifelse(.data$pat_has_visit_med75 == 1, .data$max_visit_per_pat, NA),
          na.rm = TRUE
        ),
        .by = c("study_id", "site_number", "n_pat", "n_pat_with_med75", "study_qup8_max_visit")
      ) %>%
      mutate(
        # we correct visit_med75 so that it cannot exceed a visit with less than
        # the minimum ratio of patients
        visit_med75 = ifelse(
          .data$visit_med75 > .data$study_qup8_max_visit,
          .data$study_qup8_max_visit,
          .data$visit_med75
        )
      ) %>%
      select(- "study_qup8_max_visit")
  }

  df_site <- df_site %>%
    select(c("study_id",
             "site_number",
             "n_pat",
             "n_pat_with_med75",
             "visit_med75"))

  return(df_site)
}

#' @title Evaluate sites.
#' @description Correct under-reporting probabilities using  \code{\link[stats]{p.adjust}}.
#' @param df_sim_sites dataframe generated by \code{\link[simaerep]{sim_sites}} or [sim_inframe()]
#' @param method character, passed to stats::p.adjust(), if NULL no multiplicity correction
#' will be made.
#' @inheritParams simaerep
#' @param ... use to pass r_sim_sites parameter to eval_sites_deprecated()
#' @return dataframe with the following columns:
#' \describe{
#'   \item{**study_id**}{study identification}
#'   \item{**site_number**}{site identification}
#'   \item{**visit_med75**}{median(max(visit)) * 0.75}
#'   \item{**mean_ae_site_med75**}{mean AE at visit_med75 site level}
#'   \item{**mean_ae_study_med75**}{mean AE at visit_med75 study level}
#'   \item{**pval**}{p-value as returned by \code{\link[stats]{poisson.test}}}
#'   \item{**prob**}{bootstrapped probability}
#'
#' }
#' @examples
#' df_visit <- sim_test_data_study(
#'   n_pat = 100,
#'   n_sites = 5,
#'   ratio_out = 0.4,
#'   factor_event_rate = 0.6
#'   ) %>%
#'   # internal functions require internal column names
#'   dplyr::rename(
#'     n_ae = n_event,
#'     site_number = site_id,
#'     patnum = patient_id
#'   )
#'
#' df_site <- site_aggr(df_visit)
#'
#' df_sim_sites <- sim_sites(df_site, df_visit, r = 100)
#'
#' df_eval <- eval_sites(df_sim_sites)
#' df_eval
#'
#' @rdname eval_sites
#' @seealso \code{\link[simaerep]{site_aggr}},
#' \code{\link[simaerep]{sim_sites}},
#' \code{\link[simaerep]{sim_inframe}},
#' \code{\link[stats]{p.adjust}}
#' @export
#' @keywords internal
eval_sites <- function(df_sim_sites,
                        method = "BH",
                        under_only = TRUE,
                        visit_med75 = TRUE,
                        ...) {

  if (is.na(method) || is.null(method) || method %in% c("None", "none")) {
    method <- NULL
  }

  df_out <- df_sim_sites

  # pval columns
  # pass through p_adjust not calculate over-reporting
  # column name should be pval

  if (any(endsWith(colnames(df_out), suffix = "pval"))) {
    cols_pval <- colnames(select(df_out, contains("pval")))
    warning_na(df_out, cols_pval)

    if (! is.null(method)) {
      df_out <- df_out %>%
        p_adjust(cols_pval, method = method)
    }

  }

  # under_only is only allowed for the classic algorithm
  # it can only run with one event_type with a predictable column name
  if (under_only) {

    if (! is.null(method)) {
      df_out <- df_out %>%
        p_adjust("prob_low", method = method)
    }

    df_out <- df_out %>%
      mutate(
        prob = (1 - .data$prob_low) * - 1
      ) %>%
      select(- "prob_low")

    return(df_out)

  }

  cols_prob_low <- colnames(select(df_out, contains("prob_low")))
  warning_na(df_out, cols_prob_low)

  cols_prob_or <- stringr::str_replace(cols_prob_low, "low$", "or")
  cols_prob_ur <- stringr::str_replace(cols_prob_low, "low$", "ur")

  cols_prob <- stringr::str_replace(cols_prob_low, "_low$", "")

  # prob_low is defined as the probability of getting the same
  # or lower value than the observed value. This is identical
  # with the over-reporting probability. So we rename the column

  df_out <- df_out %>%
    rename(
      setNames(cols_prob_low, cols_prob_or)
    )

  # the under-reporting probability is the inversion of the over-reporting
  # probability.

  for (i in seq_along(cols_prob_low)) {
    df_out <- df_out %>%
      mutate(
        "{cols_prob_ur[i]}" := 1 - .data[[cols_prob_or[i]]],
      )
  }

  # p_adjust() when applied to database tables can create complex
  # sql so it is best applied outside a loop

  if (! is.null(method)) {

    # p_adjust expects to lift small p-values based on multiplicity
    # for pro_cols: 0.95 = 95% = p-value 0.05, therefore we invert
    # and invert back right after

    df_out <- df_out %>%
      mutate(across(all_of(c(cols_prob_or, cols_prob_ur)), ~ 1 - .)) %>%
      p_adjust(cols_prob_or, method = method) %>%
      p_adjust(cols_prob_ur, method = method) %>%
      mutate(across(all_of(c(cols_prob_or, cols_prob_ur)), ~ 1 - .))
  }


  for (i in seq_along(cols_prob_low)) {

    # we correct for an edge case in which the quantitative count of
    # events between site and study is equal which can happen when
    # both are zero.

    # inframe and classic algorithms have different columnn names
    inframe <- ! any(str_detect(colnames(df_out), "med75"))

    if (! inframe) {
      col_quant_event_site <- "mean_ae_site_med75"
      col_quant_event_study <- "mean_ae_study_med75"
    } else {
      event <- stringr::str_split_1(cols_prob_low[i], "_")[1]
      col_quant_event_site <- paste0(event, "_per_visit_site")
      col_quant_event_study <- paste0(event, "_per_visit_study")
      col_event_delta <- paste0(event, "_delta")
    }

    col_prob_or <- cols_prob_or[i]
    col_prob_ur <- cols_prob_ur[i]
    col_prob <- cols_prob[i]

    df_out <- df_out %>%
      mutate(
        "{col_prob_or}" := ifelse(
            .data[[col_quant_event_site]] == .data[[col_quant_event_study]],
            0,
            .data[[col_prob_or]]
          ),
        # here we build col_prob
        "{col_prob}" := ifelse(
          .data[[col_prob_ur]] >= .data[[col_prob_or]],
          .data[[col_prob_ur]] * - 1,
          .data[[col_prob_or]]
          )
        )

    # when not relying on med75 we can calculate delta_event for each site
    if (! visit_med75 & inframe) {
      df_out <- df_out %>%
        mutate(
          "{col_event_delta}" := (.data[[col_quant_event_site]] * .data[["visits"]]) -
            (.data[[col_quant_event_study]] * .data[["visits"]])
        )
    }

  }

  # we only return cols_prob
  df_out <- df_out %>%
    select(- all_of(c(cols_prob_or, cols_prob_ur)))

  # order

  if (inherits(df_out, "data.frame")) {
    fun_arrange <- arrange
  } else {
    fun_arrange <- window_order
  }

  df_out <- df_out %>%
    fun_arrange(.data$study_id, .data$site_number)

  return(df_out)
}

#'@keywords internal
p_adjust <- function(df, cols, method = "BH") {


  if (inherits(df, "data.frame")) {

    df_out <- df %>%
      mutate(
        across(.cols = all_of(cols), .fns = ~p.adjust(.x, method = method)),
        .by = "study_id"
      )

  } else {

    df_out <- p_adjust_bh_inframe(df, cols)

  }

  return(df_out)

}

#'@keywords internal
warning_na <- function(df, col) {

  any_na <- get_any_na(df, col)

  if (any(any_na)) {
    warning_messages <- df %>%
      filter(dplyr::if_any(any_of(col), is.na)) %>%
      mutate(
        warning = paste0("\nstudy_id: ", .data$study_id,
                         ", site_number: ", .data$site_number, ", a prob_low value contains NA")
      ) %>%
      distinct() %>%
      pull(.data$warning) %>%
      paste(collapse = "\n")

    warning(warning_messages)
  }
}

#'@keywords internal
get_any_na <- function(df, cols) {
  # dbplyr throws a warning for not using na.rm = TRUE
  col2 <- paste0(cols, "_any_na")

  suppressWarnings({
    any_na <- df %>%
      mutate(across(
        .cols = all_of(cols), .fns = ~(ifelse(is.na(.x), 1, 0)), .names = "{col2}")
      ) %>%
      summarise(
        across(.cols = all_of(col2), .fns = ~sum(.x, na.rm = TRUE), .names = "{col2}")
      ) %>%
      mutate(
        across(.cols = all_of(col2), .fns = ~(.x > 0), .names = "{col2}"))

      any_na_list <- logical(0)

      for (col in col2){
        any_na_list <- c(any_na_list, pull(any_na, col))
      }
      return(any_na_list)
        })
  }







#' @title Create a study specific patient pool for sampling
#' @description Internal function for \code{\link[simaerep]{sim_sites}},
#' filter all visits greater than max_visit_med75_study
#' returns dataframe with one column for studies and one column with nested
#' patient data.
#' @param df_visit dataframe, created by \code{\link[simaerep]{sim_sites}}
#' @param df_site dataframe created by \code{\link[simaerep]{site_aggr}}
#' @return dataframe with nested pat_pool column
#' @examples
#' df_visit <- sim_test_data_study(
#'   n_pat = 100,
#'   n_sites = 5,
#'   ratio_out = 0.4,
#'   factor_event_rate = 0.6
#'   ) %>%
#'   # internal functions require internal column names
#'   dplyr::rename(
#'     n_ae = n_event,
#'     site_number = site_id,
#'     patnum = patient_id
#'   )
#'
#' df_site <- site_aggr(df_visit)
#'
#' df_pat_pool <- simaerep:::pat_pool(df_visit, df_site)
#'
#' df_pat_pool
#' @rdname pat_pool
#' @keywords internal
pat_pool <- function(df_visit, df_site) {
  df_site <- df_site %>%
    group_by(.data$study_id) %>%
    mutate(max_visit_med75_study = max(.data$visit_med75))

  df_visit %>%
    left_join(df_site, by = c("study_id", "site_number")) %>%
    filter(.data$visit <= .data$max_visit_med75_study) %>%
    select(c("study_id",
             "patnum",
             "visit",
             "n_ae")) %>%
    nest(pat_pool = c("patnum", "visit", "n_ae"))
}




#' @title Calculate bootstrapped probability for obtaining a lower site mean AE number.
#' @description Internal function used by [sim_sites()][sim_sites()]
#' @param site_ae vector with AE numbers
#' @param study_ae vector with AE numbers
#' @param r integer, denotes number of simulations, default = 1000
#' @param under_only compute under-reporting probabilities only, default = TRUE
#' @return pval
#' @details sets pvalue=1 if mean AE site is greater than mean AE study
#' @examples
#' simaerep:::prob_lower_site_ae_vs_study_ae(
#'   site_ae = c(5, 3, 3, 2, 1, 6),
#'   study_ae = c(9, 8, 7, 9, 6, 7, 8)
#' )
#' @seealso
#' [sim_sites()][sim_sites()]
#' @rdname prob_lower_site_ae_vs_study_ae
#' @keywords internal
prob_lower_site_ae_vs_study_ae <- function(site_ae, study_ae, r = 1000, under_only = TRUE) {
  # if there is only one site

  if (is.null(study_ae)) {
    prob_lower <- 1
    return(prob_lower)
  }

  mean_ae_site <- mean(site_ae, na.rm = TRUE)
  mean_ae_study <- mean(study_ae, na.rm = TRUE)

  # this can happen when no patients with site visit_med75 were found in study
  if (is.na(mean_ae_study)) {
    return(NA)
  }

  if (under_only) {
    # we are not interested in cases where site AE is greater study AE
    if (mean_ae_site > mean_ae_study) {
      prob_lower <- 1
      return(prob_lower)
    }
  }

  pool <- c(site_ae, study_ae)
  n_pat <- length(site_ae)

  sim <- function(seed) {
    set.seed(seed)
    me <- mean(sample(pool, n_pat, replace = TRUE))
    # '<=' includes all cases where mean_ae_site == 0 and me also == 0
    return(as.integer(ifelse(me <= mean_ae_site, 1, 0)))
  }

  df_sim <- tibble(seed = seq.int(1, r, 1)) %>%
    mutate(prob_lower = purrr::map_int(.data$seed, .f = sim)) %>%
    summarise(prob_lower = sum(.data$prob_lower) / r)

  return(df_sim$prob_lower)
}


#' @title Calculate prob_lower and poisson.test pvalue for study sites.
#' @description Collects the number of AEs of all eligible patients that
#'   meet visit_med75 criteria of site. Then  calculates poisson.test pvalue and
#'   bootstrapped probability of having a lower mean value. Used by [simaerep_classic()]
#' @param df_visit dataframe, created by \code{\link[simaerep]{sim_sites}}
#' @param df_site dataframe created by \code{\link[simaerep]{site_aggr}}
#' @param r integer, denotes number of simulations, default = 1000
#' @param poisson_test logical, calculates poisson.test pvalue
#' @param prob_lower logical, calculates probability for getting a lower value
#' @param progress logical, display progress bar, Default = TRUE
#' @param under_only compute under-reporting probabilities only, default = TRUE
#'  [check_df_visit()][check_df_visit], computationally expensive on large data
#'  sets. Default: TRUE
#' @return dataframe with the following columns:
#' \describe{
#'   \item{**study_id**}{study identification}
#'   \item{**site_number**}{site identification}
#'   \item{**n_pat**}{number of patients at site}
#'   \item{**visit_med75**}{median(max(visit)) * 0.75}
#'   \item{**n_pat_with_med75**}{number of patients at site with med75}
#'   \item{**mean_ae_site_med75**}{mean AE at visit_med75 site level}
#'   \item{**mean_ae_study_med75**}{mean AE at visit_med75 study level}
#'   \item{**n_pat_with_med75_study**}{number of patients at study with med75 excl. site}
#'   \item{**pval**}{p-value as returned by \code{\link[stats]{poisson.test}}}
#'   \item{**prob_low**}{bootstrapped probability for having mean_ae_site_med75 or lower}
#'  }
#' @examples
#' df_visit <- sim_test_data_study(
#'   n_pat = 100,
#'   n_sites = 5,
#'   ratio_out = 0.4,
#'   factor_event_rate = 0.6
#'   ) %>%
#'   # internal functions require internal column names
#'   dplyr::rename(
#'     n_ae = n_event,
#'     site_number = site_id,
#'     patnum = patient_id
#'   )
#'
#' df_site <- site_aggr(df_visit)
#'
#' df_sim_sites <- sim_sites(df_site, df_visit, r = 100)
#'
#' df_sim_sites %>%
#'  knitr::kable(digits = 2)
#' @rdname sim_sites
#' @seealso \code{\link[simaerep]{sim_sites}},
#' \code{\link[simaerep]{site_aggr}},
#' \code{\link[simaerep]{pat_pool}},
#' \code{\link[simaerep]{prob_lower_site_ae_vs_study_ae}},
#' \code{\link[simaerep]{poiss_test_site_ae_vs_study_ae}},
#' \code{\link[simaerep]{sim_sites}},
#' \code{\link[simaerep]{prep_for_sim}}
#' \code{\link[simaerep]{simaerep_classic}}
#' @export
sim_sites <- function(df_site,
                      df_visit,
                      r = 1000,
                      poisson_test = TRUE,
                      prob_lower = TRUE,
                      progress = TRUE,
                      under_only = TRUE) {

  df_sim_prep <- prep_for_sim(df_site, df_visit)

  df_sim <- sim_after_prep(df_sim_prep,
                           r = r,
                           poisson_test = poisson_test,
                           prob_lower = prob_lower,
                           progress = progress,
                           under_only = under_only)

  return(df_sim)
}

#'@title Prepare data for simulation.
#'@description Internal function called by \code{\link[simaerep]{sim_sites}}.
#'  Collect AEs per patient at visit_med75 for site and study as a vector of
#'  integers.
#'@param df_visit dataframe, created by \code{\link[simaerep]{sim_sites}}
#'@param df_site dataframe created by \code{\link[simaerep]{site_aggr}}
#'@return dataframe
#' @examples
#' df_visit <- sim_test_data_study(
#'   n_pat = 100,
#'   n_sites = 5,
#'   ratio_out = 0.4,
#'   factor_event_rate = 0.6
#'   ) %>%
#'   # internal functions require internal column names
#'   dplyr::rename(
#'     n_ae = n_event,
#'     site_number = site_id,
#'     patnum = patient_id
#'   )
#'
#' df_site <- site_aggr(df_visit)
#'
#' df_prep <- simaerep:::prep_for_sim(df_site, df_visit)
#' df_prep
#'@rdname prep_for_sim
#' @seealso \code{\link[simaerep]{sim_sites}}, \code{\link[simaerep]{sim_after_prep}}
#'@keywords internal
prep_for_sim <- function(df_site, df_visit) {

  df_pat_pool <- pat_pool(df_visit, df_site)

  df_sim_prep <- df_visit %>%
    left_join(df_site, by = c("study_id", "site_number")) %>%
    filter(.data$visit == .data$visit_med75) %>%
    group_by(.data$study_id,
             .data$site_number,
             .data$n_pat,
             .data$n_pat_with_med75,
             .data$visit_med75) %>%
    summarise(patients = list(unique(.data$patnum)), .groups = "drop")


  df_sim_prep <- df_sim_prep %>%
    left_join(df_pat_pool, "study_id") %>%
    mutate(
      pat_pool = map2(
        .data$pat_pool, .data$visit_med75,
        function(x, y) filter(x, .data$visit == y)
      ),
      n_ae_site = map2(
        .data$pat_pool, .data$patients,
        function(x, y) filter(x, .data$patnum %in% y)
      ),
      n_ae_study = map2(
        .data$pat_pool, .data$patients,
        function(x, y) filter(x, ! .data$patnum %in% y)
      ),
      n_ae_site = map(.data$n_ae_site, "n_ae"),
      n_ae_study = map(.data$n_ae_study, "n_ae")
    ) %>%
    select(- c("patients", "pat_pool"))

  return(df_sim_prep)

}

#'@title Start simulation after preparation.
#'@description Internal function called by \code{\link[simaerep]{sim_sites}}
#'  after \code{\link[simaerep]{prep_for_sim}}
#'@param df_sim_prep dataframe as returned by
#'  \code{\link[simaerep]{prep_for_sim}}
#'@inheritParams sim_sites
#'@return dataframe
#' @examples
#' df_visit <- sim_test_data_study(
#'   n_pat = 100,
#'   n_sites = 5,
#'   ratio_out = 0.4,
#'   factor_event_rate = 0.6
#'   ) %>%
#'   # internal functions require internal column names
#'   dplyr::rename(
#'     n_ae = n_event,
#'     site_number = site_id,
#'     patnum = patient_id
#'   )
#'
#' df_site <- site_aggr(df_visit)
#'
#' df_prep <- simaerep:::prep_for_sim(df_site, df_visit)
#'
#' df_sim <- simaerep:::sim_after_prep(df_prep)
#'
#' df_sim
#'@rdname sim_after_prep
#'@seealso \code{\link[simaerep]{sim_sites}},
#'  \code{\link[simaerep]{prep_for_sim}}
#'@keywords internal
sim_after_prep <- function(df_sim_prep,
                           r = 1000,
                           poisson_test = FALSE,
                           prob_lower = TRUE,
                           progress = FALSE,
                           under_only = TRUE) {
  df_sim <- df_sim_prep

  if (poisson_test) {
    df_sim <- df_sim %>%
      mutate(pval = pmap_dbl(list(.data$n_ae_site, .data$n_ae_study, .data$visit_med75),
                             poiss_test_site_ae_vs_study_ae))
  }

  if (prob_lower) {
      df_sim <- df_sim %>%
        mutate(
          prob_low = purrr::map2_dbl(
            .data$n_ae_site, .data$n_ae_study,
            ~ prob_lower_site_ae_vs_study_ae(
              .x, .y, r = .env$r, under_only = .env$under_only
            ),
            .progress = .env$progress
          )
       )
  }

  # clean
  df_sim <- df_sim %>%
    mutate(mean_ae_site_med75 = map_dbl(.data$n_ae_site, mean),
           n_pat_with_med75_study = map_int(.data$n_ae_study, length),
           # replace empty vector with NA to silence warning
           n_ae_study = ifelse(.data$n_pat_with_med75_study == 0, NA, .data$n_ae_study),
           mean_ae_study_med75 = map_dbl(.data$n_ae_study, mean)) %>%
    mutate(
      across(
        any_of(c("prob_low", "pval")),
        ~ ifelse(.data$n_pat_with_med75_study == 0, NA, .)
      )
    ) %>%
    select(- c("n_ae_site", "n_ae_study")) %>%
    select(c("study_id",
             "site_number",
             "n_pat",
             "n_pat_with_med75",
             "visit_med75",
             "mean_ae_site_med75",
             "mean_ae_study_med75",
             "n_pat_with_med75_study"),
           dplyr::everything()) %>%
    ungroup()

  df_fail <- df_sim %>%
    filter(is.na(.data$mean_ae_study_med75), .data$n_pat_with_med75_study == 0)

  if (nrow(df_fail) > 0) {
    warning_messages <- df_fail %>%
      mutate(
        warning = paste0(
          "\nstudy_id: ",
          .data$study_id,
          ", site_number: ",
          .data$site_number,
          ". No adequate patients found in study pool at visit_med75: ",
          .data$visit_med75
        )
      ) %>%
      pull(.data$warning)

    warning(warning_messages)
  }

  return(df_sim)

}



#'@title Aggregate from visit to site level.
#'@description Calculates visit_med75, n_pat_with_med75 and mean_ae_site_med75.
#'Used by [simaerep_classic()]
#'@param df_visit dataframe with columns: study_id, site_number, patnum, visit,
#'  n_ae
#'@param method character, one of c("med75", "med75_adj", "max") defining method for
#'  defining evaluation point visit_med75 (see details), Default: "med75_adj"
#'@param min_pat_pool, double, minimum ratio of available patients available for
#'  sampling. Determines maximum visit_med75 value see Details. Default: 0.2
#'@param event_names vector, contains the event names, default = "ae"
#'@details For determining the visit number at which we are going to evaluate AE
#'  reporting we take the maximum visit of each patient at the site and take the
#'  median. Then we multiply with 0.75 which will give us a cut-off point
#'  determining which patient will be evaluated. Of those patients we will
#'  evaluate we take the minimum of all maximum visits hence ensuring that we
#'  take the highest visit number possible without excluding more patients from
#'  the analysis. In order to ensure that the sampling pool for that visit is
#'  large enough we limit the visit number by the 80% quantile of maximum visits
#'  of all patients in the study. "max" will determine site max visit, flag patients
#'  that concluded max visit and count patients and patients that concluded max visit.
#'@return dataframe with the following columns: \describe{
#'  \item{**study_id**}{study identification} \item{**site_number**}{site
#'  identification} \item{**n_pat**}{number of patients, site level}
#'  \item{**visit_med75**}{adjusted median(max(visit)) * 0.75 see Details}
#'  \item{**n_pat_with_med75**}{number of patients that meet visit_med75
#'  criterion, site level} \item{**mean_ae_site_med75**}{mean AE at visit_med75,
#'  site level} }
#' @examples
#' df_visit <- sim_test_data_study(
#'   n_pat = 100,
#'   n_sites = 5,
#'   ratio_out = 0.4,
#'   factor_event_rate = 0.6
#'   ) %>%
#'   # internal functions require internal column names
#'   dplyr::rename(
#'     n_ae = n_event,
#'     site_number = site_id,
#'     patnum = patient_id
#'   )
#'
#' df_site <- site_aggr(df_visit)
#'
#' df_site %>%
#'   knitr::kable(digits = 2)
#'@rdname site_aggr
#'@seealso [simaerep_classic()]
#'@export
site_aggr <- function(df_visit,
                       method = "med75_adj",
                       min_pat_pool = 0.2,
                       event_names = c("ae")) {



  # Checks ----------------------------------------------------------
  stopifnot(
    method %in% c("med75", "med75_adj", "max")
  )


  if (method == "max") {

    df_site <- df_visit %>%
      mutate(
        max_visit = max(.data$visit, na.rm = TRUE),
        pat_has_max_visit = ifelse(visit == .data$max_visit, patnum, NA),
        .by = c("study_id", "site_number")
      ) %>%
      summarise(
        n_pat = n_distinct(.data$patnum),
        n_pat_with_max_visit = n_distinct(.data$pat_has_max_visit),
        .by = c("study_id", "site_number", "max_visit")
      )

    return(df_site)
  }

  # Aggregate on patient level---------------------------------------


  df_pat <- pat_aggr(df_visit)

  # Aggregate on site level ------------------------------------------

  df_site <- get_visit_med75(df_pat, method = method, min_pat_pool = min_pat_pool)

  # Calculate mean cumulative AE at med75 per Site ------------------

  colnames <- paste0("mean_", event_names, "_site_med75")
  colsearch <- paste0("mean_", event_names, "_site")



  df_mean_ae_dev <- get_site_mean_ae_dev(df_visit, df_pat, df_site, event_names = event_names)

  df_mean_ae_med75 <- df_mean_ae_dev %>%
    filter(.data$visit == .data$visit_med75)  %>%
    dplyr::rename_with(~ colnames[which(colsearch == .x)], .cols = all_of(colsearch)) %>%
    select(c("study_id",
             "site_number",
             all_of(colnames)))
  # Add mean cumulative AE to site aggregate ----------------------

  df_site <- df_site %>%
    left_join(df_mean_ae_med75, by = c("study_id", "site_number")) %>%
    ungroup()

  return(df_site)
}



#' @title Poisson test for vector with site AEs vs vector with study AEs.
#' @description Internal function used by [simaerep][sim_sites()].
#' @param site_ae vector with AE numbers
#' @param study_ae vector with AE numbers
#' @param visit_med75 integer
#' @return pval
#' @details sets pvalue=1 if mean AE site is greater than mean AE study or ttest gives error
#' @examples
#' simaerep:::poiss_test_site_ae_vs_study_ae(
#'    site_ae = c(5, 3, 3, 2, 1, 6),
#'    study_ae = c(9, 8, 7, 9, 6, 7, 8),
#'    visit_med75 = 10
#')
#'
#' simaerep:::poiss_test_site_ae_vs_study_ae(
#'    site_ae = c(11, 9, 8, 6, 3),
#'    study_ae = c(9, 8, 7, 9, 6, 7, 8),
#'    visit_med75 = 10
#')
#' @seealso [sim_sites()][sim_sites()]
#' @rdname poiss_test_site_ae_vs_study_ae
#' @keywords internal

poiss_test_site_ae_vs_study_ae <- function(site_ae,
                                           study_ae,
                                           visit_med75) {

  # if there is only one site
  if (is.null(study_ae)) {
    pval <- 1
    return(pval)
  }

  mean_ae_site <- mean(site_ae, na.rm = TRUE)
  mean_ae_study <- mean(study_ae, na.rm = TRUE)

  # this can happen when no patients with site visit_med75 were found in study
  if (is.na(mean_ae_study)) {
    return(NA)
  }

  # we are not interested in cases where site AE is greater study AE
  if (mean_ae_site > mean_ae_study) {
    pval <- 1
    return(pval)
  }

  n_pat_site <- length(site_ae)
  n_pat_study <- length(study_ae)

  time_base_site <- n_pat_site * visit_med75
  time_base_study <- n_pat_study * visit_med75

  n_events_site <- sum(site_ae)
  n_events_study <- sum(study_ae)

  safely_poisson_test <- purrr::safely(poisson.test)

  poisson_test <- safely_poisson_test(c(n_events_site, n_events_study),
                                      c(time_base_site, time_base_study))

  pval <- poisson_test$result$p.value

  # this controls for cases when poisson.test fails for some reason
  if (is.null(pval)) {
    pval <- 1
  }

  return(pval)
}
