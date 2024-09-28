


#' prune visits to visit_med75 using table operations
#'@inheritParams simaerep
#'@param df_site dataframe, as returned by [site_aggr()]
#'@keywords internal
prune_to_visit_med75_inframe <- function(df_visit, df_site) {
  df_pat_aggr <- pat_aggr(df_visit)

  # drop patients that did not reach visit_med75
  df_pat <- df_pat_aggr %>%
    left_join(
      df_site,
      by = c("study_id", "site_number")
    ) %>%
    filter(.data$max_visit_per_pat >= .data$visit_med75) %>%
    select(c("study_id", "site_number", "patnum", "visit_med75"))

  # filter patients and drop excess visits
  df_visit <- df_visit %>%
    inner_join(
      df_pat,
      by = c("study_id", "site_number", "patnum")
    ) %>%
    filter(.data$visit <= .data$visit_med75) %>%
    select(- c("visit_med75"))
}


#' Calculate prob_lower for study sites using table operations
#'@export
#'@inheritParams simaerep
#'@param df_site, dataframe as returned be [site_aggr()], Will switch to visit_med75.
#'Default: NULL
#'@examples
#' df_visit <- sim_test_data_study(
#'   n_pat = 100,
#'   n_sites = 5,
#'   frac_site_with_ur = 0.4,
#'   ur_rate = 0.6
#' )
#' df_visit$study_id <- "A"
#'
#' df_sim <- sim_inframe(df_visit)
#' df_eval <- eval_sites(df_sim)
#' df_eval
sim_inframe <- function(df_visit, r = 1000, df_site = NULL) {

  # db back-end, to not form ratios from integers
  df_visit <- df_visit %>%
    mutate(
      n_ae = as.double(.data$n_ae),
      visit = as.double(.data$visit)
    )

  if (! inherits(r, "tbl")) {
    df_r <- tibble(rep = seq(1, r))
  } else {
    df_r <- r %>%
      # here we make sure that any input dataframe works
      # we use as many replications as we have rows
      rename(rep = 1) %>%
      mutate(
        rep = row_number()
      )
  }

  # aggregate per patient to get max visits
  df_pat_aggr_pool <- pat_aggr(df_visit)

  # this implements visit_med75
  if (! is.null(df_site)) {
    df_visit_prune <- prune_to_visit_med75_inframe(df_visit, df_site)

    df_calc_ori <- df_visit_prune %>%
      filter(visit == max(.data$visit, na.rm = TRUE), .by = c("patnum", "study_id")) %>%
      summarise(
        events = sum(.data$n_ae),
        visits = sum(.data$visit),
        n_pat = n_distinct(.data$patnum),
        events_per_visit_site_ori = sum(.data$n_ae, na.rm = TRUE) / sum(.data$visit, na.rm = TRUE),
        .by = c("study_id", "site_number")
      )

    df_pat_aggr_site <- df_visit_prune %>%
      prune_to_visit_med75_inframe(df_site) %>%
      pat_aggr()

    remove(df_visit_prune)

  } else {
    df_pat_aggr_site <- df_pat_aggr_pool

    df_calc_ori <- df_visit %>%
      filter(visit == max(.data$visit, na.rm = TRUE), .by = c("patnum", "study_id")) %>%
      summarise(
        events = sum(.data$n_ae),
        visits = sum(.data$visit),
        n_pat = n_distinct(.data$patnum),
        events_per_visit_site_ori = sum(.data$n_ae, na.rm = TRUE) / sum(.data$visit, na.rm = TRUE),
        .by = c("study_id", "site_number")
      )
  }

  # for every max visit in the data add all eligible patients
  # assign a number to each patient
  df_visit_pat_pool <- df_pat_aggr_pool %>%
    distinct(.data$study_id, visit = .data$max_visit_per_pat) %>%
    left_join(
      df_pat_aggr_pool,
      by = join_by(
        "visit" <= "max_visit_per_pat",
        "study_id" == "study_id"
      )
    ) %>%
    mutate(
      rwn = row_number(),
      .by = c("study_id", "visit")
    ) %>%
    mutate(
      max_rwn = max(.data$rwn, na.rm = TRUE),
      .by = c("study_id", "visit")
    )

  # for each site create repetitions using cross join
  # add patients with maximum visit back to each site
  df_sim_prep <- df_pat_aggr_site %>%
    distinct(.data$study_id, .data$site_number) %>%
    cross_join(df_r) %>%
    left_join(
      df_pat_aggr_site,
      by = c("study_id", "site_number"),
      relationship = "many-to-many"
    ) %>%
  # add number of eligible patients from pool
    left_join(
      df_visit_pat_pool %>%
        distinct(.data$study_id, .data$visit, .data$max_rwn),
      by = c("study_id", max_visit_per_pat = "visit")
    ) %>%
    mutate(
      # we generate a random number between 1 and the maximum number
      # of eligible patients
      rnd = runif(n())
    )

  if (inherits(df_sim_prep, "tbl_Snowflake")) {
    # snowflake RANDOM() works differently than other backends and returns large
    # positive and negative integers. We normalize by using the min and max values
    # of those 64bit integers. Source for integer values is ChatGPT.
    # for a range from -5 to 5: 4 -> 0.0  -4 -> 0.1, with underlying formula

    df_sim_prep <- df_sim_prep %>%
      mutate(
        rnd = (.data$rnd - (-9223372036854775808)) / (9223372036854775807 - (-9223372036854775808))
      )
  }

  df_sim_prep <- df_sim_prep %>%
    mutate(
      # transform to values between 1 and max number of patients
      rnd_rwn = floor(.data$rnd * .data$max_rwn) + 1
    )

  df_sim <- df_sim_prep %>%
    left_join(
      df_visit_pat_pool %>%
        select(c("study_id", "visit", patnum_new = "patnum", "rwn")),
      by = c(study_id = "study_id", max_visit_per_pat = "visit", rnd_rwn = "rwn")
    ) %>%
    select(c("study_id", "site_number", "rep", patnum = "patnum_new", visit = "max_visit_per_pat"))


  # add appropriate visit with cumulative event and visit count for every new patient
  df_calc <- df_sim %>%
    left_join(
      df_visit %>%
        select(c("study_id", "patnum", "visit", "n_ae")),
      by = c("study_id", "patnum", "visit")
    ) %>%
    # calculate site level event rates for every repetition
    summarise(
      events_per_visit_site_rep = sum(.data$n_ae, na.rm = TRUE) / sum(.data$visit, na.rm = TRUE),
      .by = c("study_id", "rep", "site_number")
    )

  # join original with simulations
  df_calc_aggr <- df_calc %>%
    left_join(
      df_calc_ori,
      by = c("study_id", "site_number")
    ) %>%
    summarise(
      # calculate probability by comparing original stats with simulated stats
      # how many times simulated value below original value
      prob_low = sum(
        ifelse(
          .data$events_per_visit_site_rep <= .data$events_per_visit_site_ori,
          1, 0
          ),
        na.rm = TRUE
      ) / n(),
      events_per_visit_study = mean(.data$events_per_visit_site_rep, na.rm = TRUE),
      .by = c("study_id", "site_number", "events_per_visit_site_ori", "events",
              "visits", "n_pat")
    ) %>%
    rename(events_per_visit_site = "events_per_visit_site_ori")

  return(df_calc_aggr)
}

#' benjamini hochberg p value correction using table operations
#'@keywords internal
p_adjust_bh_inframe <- function(df_eval, col, suffix) {

  any_probx <- any(str_detect(colnames(df_eval), "probx_"))

  stopifnot("probx_ prefix in colnames(df_eval) not allowed" = ! any_probx)

  if (inherits(df_eval, "data.frame")) {
    fun_arrange <- arrange
  } else {
    fun_arrange <- window_order
  }

  col_adj <- paste0(col, "_adj")
  col_suffix <- paste0(col, suffix)

  df_out <- df_eval %>%
    mutate(probx = .data[[col]]) %>%
    fun_arrange(.data$study_id, .data$probx) %>%
    max_rank("probx", "probx_n_detected") %>%
    mutate(
      probx_min = min(ifelse(.data$probx == 0, NA, .data$probx), na.rm = TRUE),
      .by = "study_id"
    ) %>%
    mutate(
      probx_fp = .data$probx * n(),
      probx_fp = ifelse(.data$probx_fp == 0, .data$probx_min / 10, .data$probx_fp)
    ) %>%
    mutate(
      probx_p_vs_fp_ratio = .data$probx_n_detected / .data$probx_fp,
      probx_p_vs_fp_ratio = ifelse(is.na(.data$probx_p_vs_fp_ratio),
                                      0, .data$probx_p_vs_fp_ratio),
      # it is possible to get ratios lower than one if p < expected fp
      # this can happen by chance and is meaningless
      probx_p_vs_fp_ratio = ifelse(.data$probx_p_vs_fp_ratio < 1, 1, .data$probx_p_vs_fp_ratio)
    ) %>%
    mutate(
      !! as.name(col_adj) := 1 / .data$probx_p_vs_fp_ratio,
      !! as.name(col_suffix) := 1 - .data[[col_adj]]
    ) %>%
    select(- starts_with("probx"))

  return(df_out)
}


#' Calculate Max Rank
#' @description
#' like rank() with ties.method = "max", works on tbl objects
#' @param df dataframe
#' @param col character column name to rank y
#' @param col_new character column name for rankings
#' @details
#' this is needed for hochberg p value adjustment. We need to assign higher
#' rank when multiple sites have same p value
#'
#' @export
#' @examples
#'
#'df <- tibble::tibble(s = c(1, 2, 2, 2, 5, 10)) %>%
#'  dplyr::mutate(
#'    rank = rank(s, ties.method = "max")
#'  )
#'
#'df %>%
#'  max_rank("s", "max_rank")
#'\donttest{
#'# Database
#'con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
#'
#'dplyr::copy_to(con, df, "df")
#'max_rank(dplyr::tbl(con, "df"), "s", "max_rank")
#'
#'DBI::dbDisconnect(con)
#'}
max_rank <- function(df, col, col_new) {

  any_na <- get_any_na(df, col)

  stopifnot("NA in max_rank col" = ! any_na)

  df %>%
    mutate(
      rwn = row_number(),
    ) %>%
    mutate(
      !! as.name(col_new) := max(.data$rwn, na.rm = TRUE),
      .by = all_of(col)
    ) %>%
    select(- "rwn")
}
