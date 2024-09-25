
new_simaerep <- function(visit,
                         df_site,
                         df_sim_sites,
                         df_eval,
                         r,
                         visit_med75,
                         inframe,
                         under_only,
                         param_site_aggr,
                         param_sim_sites,
                         param_eval_sites) {

  structure(
    list(
      visit = visit,
      df_site = df_site,
      df_sim_sites = df_sim_sites,
      df_eval = df_eval,
      r = r,
      visit_med75 = visit_med75,
      inframe = inframe,
      under_only = under_only,
      param_site_aggr = param_site_aggr,
      param_sim_sites = param_sim_sites,
      param_eval_sites = param_eval_sites
    ),
    class = "simaerep"
  )
}

validate_simaerep <- function(x) {

  obj_names <- sort(attributes(x)$names)

  obj_names_check <- sort(
    c(
      "visit",
      "df_site",
      "df_sim_sites",
      "df_eval",
      "r",
      "visit_med75",
      "inframe",
      "under_only",
      "param_site_aggr",
      "param_sim_sites",
      "param_eval_sites"
    )
  )

  stopifnot(all(obj_names == obj_names_check))

  stopifnot(is_orivisit(x$visit))
  stopifnot(is.data.frame(x$df_site) | inherits(x$df_site, "tbl"))
  stopifnot(is.data.frame(x$df_sim_sites) | inherits(x$df_sim_sites, "tbl"))
  stopifnot(is.data.frame(x$df_eval) | inherits(x$df_eval, "tbl"))
  stopifnot(is.list(x$param_site_aggr))
  stopifnot(is.list(x$param_sim_sites))
  stopifnot(is.list(x$param_eval_sites))

  return(x)
}

#'@title Create simaerep object
#'@description Simulate AE under-reporting probabilities.
#'@param df_visit Data frame with columns: study_id, site_number, patnum, visit,
#'  n_ae.
#'@param r Integer or tbl_object, number of repetitions for bootstrap
#'  simulation. Pass a tbl object referring to a table with one column and as
#'  many rows as desired repetitions. Default: 1000.
#'@param check Logical, perform data check and attempt repair with
#'  [check_df_visit()]. Computationally expensive on large data sets. Default:
#'  TRUE.
#'@param visit_med75 Logical, should evaluation point visit_med75 be used.
#'  Default: TRUE.
#'@param inframe Logical, only table operations to be used; does not require
#'  visit_med75. Compatible with dbplyr supported database backends.
#'@param mult_corr Logical, multiplicity correction, Default: TRUE
#'@param param_site_aggr List of parameters passed to [site_aggr()]. Default:
#'  list(method = "med75_adj", min_pat_pool = 0.2).
#'@param param_sim_sites List of parameters passed to [sim_sites()]. Default:
#'  list(r = 1000, poisson_test = FALSE, prob_lower = TRUE).
#'@param param_eval_sites List of parameters passed to [eval_sites()]. Default:
#'  list(method = "BH").
#'@param progress Logical, display progress bar. Default: TRUE.
#'@param env Optional, provide environment of original visit data. Default:
#'  parent.frame().
#'@param under_only Logical, compute under-reporting probabilities only.
#'  Supersedes under_only parameter passed to [eval_sites()] and [sim_sites()].
#'  Default: TRUE.
#'@return A simaerep object.
#'@details Executes [site_aggr()], [sim_sites()], and [eval_sites()] on original
#'  visit data and stores all intermediate results. Stores lazy reference to
#'  original visit data for facilitated plotting using generic plot(x).
#' @examples
#' df_visit <- sim_test_data_study(
#'   n_pat = 100,
#'   n_sites = 5,
#'   frac_site_with_ur = 0.4,
#'   ur_rate = 0.6
#' )
#' df_visit$study_id <- "A"
#' aerep <- simaerep(df_visit)
#' aerep
#' str(aerep)
#' \donttest{
#'   # In-frame table operations
#'   simaerep(df_visit, inframe = TRUE, visit_med75 = FALSE, under_only = FALSE)$df_eval
#'   simaerep(df_visit, inframe = TRUE, visit_med75 = TRUE, under_only = FALSE)$df_eval
#'   # Database example
#'   con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
#'   df_r <- tibble::tibble(rep = seq(1, 1000))
#'   dplyr::copy_to(con, df_visit, "visit")
#'   dplyr::copy_to(con, df_r, "r")
#'   tbl_visit <- dplyr::tbl(con, "visit")
#'   tbl_r <- dplyr::tbl(con, "r")
#'   simaerep(tbl_visit, r = tbl_r, inframe = TRUE, visit_med75 = FALSE, under_only = FALSE)$df_eval
#'   simaerep(tbl_visit, r = tbl_r, inframe = TRUE, visit_med75 = TRUE, under_only = FALSE)$df_eval
#'   DBI::dbDisconnect(con)
#' }
#'@seealso [site_aggr()], [sim_sites()], [eval_sites()], [orivisit()],
#'  [plot.simaerep()]
#'@export
#'@seealso [site_aggr()][site_aggr], [sim_sites()][sim_sites],
#'  [eval_sites()][eval_sites], [orivisit()][orivisit],
#'  [plot.simaerep()][plot.simaerep]
#'@rdname simaerep
#'@export
simaerep <- function(df_visit,
                     r = 1000,
                     check = TRUE,
                     under_only = TRUE,
                     visit_med75 = TRUE,
                     inframe = FALSE,
                     progress = TRUE,
                     mult_corr = TRUE,
                     param_site_aggr = list(
                       method = "med75_adj",
                       min_pat_pool = 0.2
                     ),
                     param_sim_sites = list(
                       r = 1000,
                       poisson_test = FALSE,
                       prob_lower = TRUE
                     ),
                     param_eval_sites = list(
                       method = "BH"
                     ),
                     env = parent.frame()) {

  call <- rlang::enexpr(df_visit)

  # when two tbl objects passed automatically switch to inframe
  is_tbl_df_visit <- ! is.data.frame(df_visit) & inherits(df_visit, "tbl")
  is_tbl_r <- ! is.data.frame(r) & inherits(r, "tbl")

  if (is_tbl_df_visit && is_tbl_r) {
    inframe <- TRUE
  }

  # save visit call
  visit <- tryCatch({
    visit <- orivisit(df_visit, call, env = env)
    as.data.frame(visit)
    visit},
    error = function(e) df_visit
  )

  param_sim_sites$under_only <- under_only
  param_eval_sites$under_only <- under_only

  if (! mult_corr) {
    param_eval_sites$method <- NA
  }

  if (visit_med75 && ! inframe) {

    if (r != param_sim_sites$r) {
      param_sim_sites$r <- r
      warning("r not equal param_sim_sites$r, overriding param_sim_sites$r")
    }

    aerep <- simaerep_visit_med75(
      df_visit = visit,
      under_only = under_only,
      progress = progress,
      param_site_aggr = param_site_aggr,
      param_sim_sites = param_sim_sites,
      param_eval_sites = param_eval_sites,
      env = env,
      check = check
    )

  } else if (inframe) {
    aerep <- simaerep_inframe(
      df_visit = visit,
      r = r,
      under_only = under_only,
      visit_med75 = visit_med75,
      param_site_aggr = param_site_aggr,
      param_eval_sites = param_eval_sites,
      env = env,
      check = check
    )
  } else {
    stop("visit_med75 parameter mus be TRUE if inframe is FALSE")
  }

  return(aerep)

}

#' simulate in dataframe
#' @inheritParams simaerep
#' @keywords internal
#' @export
#' @examples
#' df_visit <- sim_test_data_study(
#'  n_pat = 100,
#'  n_sites = 5,
#'  frac_site_with_ur = 0.4,
#'  ur_rate = 0.6
#' )
#' df_visit$study_id <- "A"
#'
#' simaerep_inframe(df_visit)
#' simaerep_inframe(df_visit, visit_med75 = TRUE)$df_eval
#'\donttest{
#'# Database
#'con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
#'df_r <- tibble::tibble(rep = seq(1, 1000))
#'
#'dplyr::copy_to(con, df_visit, "visit")
#'dplyr::copy_to(con, df_r, "r")
#'
#'tbl_visit <- dplyr::tbl(con, "visit")
#'tbl_r <- dplyr::tbl(con, "r")
#'
#'simaerep_inframe(tbl_visit, r = tbl_r)$df_eval
#'simaerep_inframe(tbl_visit, r = tbl_r, visit_med75 = TRUE)$df_eval
#'
#'DBI::dbDisconnect(con)
#'}
simaerep_inframe <- function(df_visit,
                             r = 1000,
                             under_only = FALSE,
                             visit_med75 = FALSE,
                             check = TRUE,
                             param_site_aggr = list(
                               method = "med75_adj",
                               min_pat_pool = 0.2
                             ),
                             param_eval_sites = list(
                               method = "BH"
                             ),
                             env = parent.frame()) {

  if (inherits(df_visit, "orivisit")) {
    visit <- df_visit
    df_visit <- as.data.frame(df_visit, env = env)
  } else {
    call <- rlang::enexpr(df_visit)
    visit <- orivisit(df_visit, call, env = env)
  }

  # check
  if (check) {
    df_visit <- check_df_visit(df_visit)
  }

  if (! visit_med75) {
    param_site_aggr$method <- "max"
  }

  df_site <- do.call(
    site_aggr,
    c(
      list(df_visit = df_visit),
      check = FALSE,
      param_site_aggr
    )
  )

  if (visit_med75) {
    df_sim_sites <- sim_inframe(df_visit, r = r, df_site = df_site)
  } else {
    df_sim_sites <- sim_inframe(df_visit, r = r)
  }

  # evaluate
  df_eval <- do.call(
    eval_sites,
    c(
      list(df_sim_sites = df_sim_sites),
      param_eval_sites
    )
  )

  if (visit_med75) {
    df_eval <- df_eval %>%
      left_join(
        df_site %>%
          select(- "n_pat"),
        by = c("study_id", "site_number")
      ) %>%
      select(- "mean_ae_site_med75")
  }

  validate_simaerep(
    new_simaerep(
      visit = visit,
      df_site,
      df_sim_sites,
      df_eval,
      r = r,
      visit_med75 = visit_med75,
      inframe = TRUE,
      under_only = under_only,
      param_site_aggr = param_site_aggr,
      param_sim_sites = list(),
      param_eval_sites = param_eval_sites
    )
  )
}

simaerep_visit_med75 <- function(df_visit,
                     param_site_aggr = list(
                       method = "med75_adj",
                       min_pat_pool = 0.2
                      ),
                     param_sim_sites = list(
                       r = 1000,
                       poisson_test = FALSE,
                       prob_lower = TRUE
                     ),
                     param_eval_sites = list(
                       method = "BH"
                     ),
                     check = TRUE,
                     progress = TRUE,
                     env = parent.frame(),
                     under_only = TRUE,
                     r = 1000) {

  if (inherits(df_visit, "orivisit")) {
    visit <- df_visit
    df_visit <- as.data.frame(df_visit, env = env)
  } else {
    call <- rlang::enexpr(df_visit)
    visit <- orivisit(df_visit, call, env = env)
  }

  # check
  if (check) {
    df_visit <- check_df_visit(df_visit)
  }

  param_sim_sites$under_only <- under_only
  param_eval_sites$under_only <- under_only

  df_site <- do.call(
    site_aggr,
    c(
      list(df_visit = df_visit),
      check = FALSE,
      param_site_aggr
    )
  )

  df_sim_sites <- do.call(
    sim_sites,
    c(
      list(
        df_site = df_site,
        df_visit = df_visit
      ),
      check = FALSE,
      progress = progress,
      param_sim_sites
    )
  )

  df_eval <- do.call(
    eval_sites,
    c(
      list(df_sim_sites = df_sim_sites),
      param_eval_sites
    )
  )

  validate_simaerep(
    new_simaerep(
      visit = visit,
      df_site,
      df_sim_sites,
      df_eval,
      r = param_sim_sites$r,
      visit_med75 = TRUE,
      inframe = FALSE,
      under_only = under_only,
      param_site_aggr = param_site_aggr,
      param_sim_sites = param_sim_sites,
      param_eval_sites = param_eval_sites
    )
  )
}

#' @title plot AE under-reporting simulation results
#' @description generic plot function for simaerep objects
#' @param x simaerep object
#' @param ... additional parameters passed to [plot_study()][plot_study] or
#'   [plot_visit_med75()][plot_visit_med75]
#' @param study character specifying study to be plotted, Default: NULL
#' @param what one of c("ur", "med75"), specifying whether to plot site AE
#'   under-reporting or visit_med75 values, Default: 'ur'
#' @param n_sites number of sites to plot, Default: 16
#' @param df_visit optional, pass original visit data if it cannot be retrieved
#'   from parent environment, Default: NULL
#' @param env optional, pass environment from which to retrieve original visit
#'   data, Default: parent.frame()
#' @return ggplot object
#' @details see [plot_study()][plot_study] and
#'   [plot_visit_med75()][plot_visit_med75]
#' @examples
#' \donttest{
#' df_visit <- sim_test_data_study(
#'   n_pat = 100,
#'   n_sites = 5,
#'   frac_site_with_ur = 0.4,
#'   ur_rate = 0.6
#' )
#'
#' df_visit$study_id <- "A"
#'
#' aerep <- simaerep(df_visit)
#'
#' plot(aerep, what = "ur", study = "A")
#' plot(aerep, what = "med75", study = "A")
#' }
#' @rdname plot.simaerep
#' @export
plot.simaerep <- function(x,
                          ...,
                          study = NULL,
                          what = "ur",
                          n_sites = 16,
                          df_visit = NULL,
                          env = parent.frame()) {

  stopifnot(what %in% c("ur", "med75"))

  .f <- switch(what,
    "ur" = plot_simaerep_plot_study,
    "med75" = plot_simaerep_plot_visit_med75
  )

  if (is.null(study)) {

    studies <- x$df_eval %>%
      distinct(.data$study_id) %>%
      pull(.data$study_id) %>%
      sort()

    study <- studies[1]

    message(paste0("study = NULL, defaulting to study:", study))
  }

  if (is.null(df_visit)) {
    df_visit <- as.data.frame(x$visit, env = env)
  }

  p <- .f(df_visit, x, study, n_sites, ...)

  return(p)

}

plot_simaerep_plot_study <- function(df_visit, x, study, n_sites, ...) {
  plot_study(
    df_visit = df_visit,
    df_site = x$df_site,
    df_eval = x$df_eval,
    study = study,
    n_sites = n_sites,
    ...
  )
}

plot_simaerep_plot_visit_med75 <- function(df_visit, x, study, n_sites, ...) {
  plot_visit_med75(
    df_visit = df_visit,
    study_id_str = study,
    n_sites = n_sites,
    min_pat_pool = x$param_site_aggr$min_pat_pool,
    ...
  )
}

#' @export
print.simaerep <- function(x, ...) {
  cat(
    paste(
      c(
        "simaerep object:",
        "Check aerep$df_eval prob_low_prob_ur column for under-reporting probabililty.",
        "Plot results using plot() generic."
      ),
      collapse = "\n"
    )
  )
}

#' @title is simaerep class
#' @description internal function
#' @param x object
#' @return logical
#' @rdname is_simaerep
#' @export
is_simaerep <- function(x) {
  "simaerep" %in% class(x)
}
