


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


#' Calculate prob for study sites using table operations
#'@keywords internal
#'@inheritParams simaerep
#'@param df_site, dataframe as returned be [site_aggr()], Will switch to visit_med75.
#'Default: NULL
#'@examples
#' df_visit <- sim_test_data_study(
#'   n_pat = 100,
#'   n_sites = 5,
#'   ratio_out = 0.4,
#'   factor_event_rate = - 0.6
#' ) %>%
#' dplyr::rename(
#'   site_number = site_id,
#'   patnum = patient_id,
#'   n_ae = n_event
#' )
#'
#' df_sim <- simaerep:::sim_inframe(df_visit)
sim_inframe <- function(df_visit, r = 1000, df_site = NULL, event_names = c("ae")) {
  colnames <- paste0("n_", event_names)



  # db back-end, to not form ratios from integers
  df_visit <- df_visit %>%
    mutate(
      across(as.double, .cols = all_of({
        colnames}), .names = "{colnames}"),
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

  colnames_event <- paste0(event_names, "_count")
  colnames_low <- paste0(event_names, "_prob_low")
  colnames_ori <- paste0(event_names, "_per_visit_site_ori")
  colnames_rep <- paste0(event_names, "_per_visit_rep")
  colnames_study <- paste0(event_names, "_per_visit_study")
  colnames_site <- paste0(event_names, "_per_visit_site")

  # this implements visit_med75
  if (! is.null(df_site)) {

    df_visit_prune <- prune_to_visit_med75_inframe(df_visit, df_site)

    df_calc_ori <- df_visit_prune %>%
      filter(visit == max(.data$visit, na.rm = TRUE), .by = c("patnum", "study_id")) %>%
      summarise(
        across(.cols = all_of({{colnames}}), .fns = sum, .names = "{colnames_event}"),
        visits = sum(.data$visit),
        n_pat = n_distinct(.data$patnum),
        .by = c("study_id", "site_number")
        ) %>%
      mutate(across(.cols = all_of(colnames_event), .fns = ~(.x / visits), .names = "{colnames_ori}"))

    df_pat_aggr_site <- df_visit_prune %>%
      prune_to_visit_med75_inframe(df_site) %>%
      pat_aggr()

    remove(df_visit_prune)

  } else {
      df_pat_aggr_site <- df_pat_aggr_pool
      df_calc_ori <- df_visit %>%
        filter(visit == max(.data$visit, na.rm = TRUE), .by = c("patnum", "study_id"))  %>%
        summarise(
          across(.cols = all_of({{colnames}}), .fns = sum, .names = "{colnames_event}"),
          visits = sum(.data$visit),
          n_pat = n_distinct(.data$patnum),
          .by = c("study_id", "site_number")
        ) %>%
        mutate(across(.cols = all_of(colnames_event), .fns = ~(.x / visits), .names = "{colnames_ori}"))
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
        select(c("study_id", "patnum", "visit", all_of(colnames))),
      by = c("study_id", "patnum", "visit")
    ) %>%
    # calculate site level event rates for every repetition
    summarise(
      across(.cols = all_of(colnames), .fns = ~sum(.x, na.rm = TRUE) / sum(visit, na.rm = TRUE),
             .names = "{colnames_rep}"),
      .by = c("study_id", "rep", "site_number")
    )





  join_temp <- df_calc %>%
    left_join(
      df_calc_ori,
      by = c("study_id", "site_number"))

  # join original with simulations
  df_calc_aggr <- df_calc %>%
    left_join(
      df_calc_ori,
      by = c("study_id", "site_number")
    ) %>%

    # calculate probability by comparing original stats with simulated stats
    # how many times simulated value below original value

    summarise(
      across(.cols = all_of(colnames_rep), .fns = ~mean(.x, na.rm = TRUE), .names = "{colnames_study}"),
      .by = c("study_id", "site_number", all_of(colnames_event), all_of(colnames_ori), "visits", "n_pat")
    )
  for (x in seq_along(colnames_ori)){
    temp <- join_temp %>%
      summarise("{colnames_low[x]}" := sum(ifelse(.data[[colnames_ori[x]]] >= .data[[colnames_rep[x]]], 1, 0)) / n(),
                .by = c("study_id", "site_number", "visits", "n_pat")) %>%
      select(c("study_id", "site_number", 5))
    df_calc_aggr <- df_calc_aggr %>%
      left_join(temp, by = c("study_id", "site_number"))
  }

  df_calc_aggr <- df_calc_aggr %>%
    dplyr::rename_with(.cols = all_of(colnames_ori), ~ colnames_site[which(colnames_ori == .x)])

  return(df_calc_aggr)
}

#' benjamini hochberg p value correction using table operations
#'@keywords internal
p_adjust_bh_inframe <- function(df_eval, cols) {

  any_probx <- any(str_detect(colnames(df_eval), "probx_"))

  stopifnot("probx_ prefix in colnames(df_eval) not allowed" = ! any_probx)

  if (inherits(df_eval, "data.frame")) {
    fun_arrange <- arrange
  } else {
    fun_arrange <- window_order
  }

  df_out <- df_eval

  for (col in cols) {
    df_out <- df_out %>%
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
        !! as.name(col) := 1 / .data$probx_p_vs_fp_ratio,
      ) %>%
      select(- starts_with("probx"))

}
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
#' @keywords internal
#' @examples
#'
#'df <- tibble::tibble(s = c(1, 2, 2, 2, 5, 10)) %>%
#'  dplyr::mutate(
#'    rank = rank(s, ties.method = "max")
#'  )
#'
#'df %>%
#'  simaerep:::max_rank("s", "max_rank")
#'\donttest{
#'# Database
#'con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
#'
#'dplyr::copy_to(con, df, "df")
#'simaerep:::max_rank(dplyr::tbl(con, "df"), "s", "max_rank")
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
