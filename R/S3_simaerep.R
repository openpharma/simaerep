
new_simaerep <- function(visit,
                         df_site,
                         df_sim_sites,
                         df_eval,
                         r,
                         visit_med75,
                         inframe,
                         under_only,
                         event_names = "ae",
                         mult_corr,
                         poisson_test) {

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
      event_names = event_names,
      mult_corr = mult_corr,
      poisson_test = poisson_test
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
      "event_names",
      "mult_corr",
      "poisson_test"
    )
  )

  stopifnot(all(obj_names == obj_names_check))

  stopifnot(is_orivisit(x$visit))
  stopifnot(is.data.frame(x$df_site) | inherits(x$df_site, "tbl"))
  stopifnot(is.data.frame(x$df_sim_sites) | inherits(x$df_sim_sites, "tbl"))
  stopifnot(is.data.frame(x$df_eval) | inherits(x$df_eval, "tbl"))

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
#'@param progress Logical, display progress bar. Default: TRUE.
#'@param env Optional, provide environment of original visit data. Default:
#'  parent.frame().
#'@param under_only Logical, compute under-reporting probabilities only.
#'  Supersedes under_only parameter passed to [eval_sites()] and [sim_sites()].
#'  Default: TRUE.
#'@param poisson_test logical, compute p-value with poisson test, only supported
#'by the classic algorithm using visit_med75.
#'@param event_names vector, contains the event names, default = "ae"
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
#'
#' # simaerep classic algorithm
#'
#' aerep <- simaerep(df_visit, inframe = FALSE, under_only = TRUE, mult_corr = TRUE)
#' str(aerep)
#'
#' # multiple events
#'
#' df_visit_events_test <- sim_test_data_events(
#'   n_pat = 100,
#'   n_sites = 5,
#'   ae_per_visit_mean = c(0.4, 0.5),
#'   event_names = c("ae", "pd")
#' )
#'
#' df_visit_events_test
#'
#' aerep_events <- simaerep(df_visit_events_test, inframe = TRUE, event_names = c("ae", "pd"))
#'
#' aerep_events
#'
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
                      under_only = FALSE,
                      visit_med75 = FALSE,
                      inframe = TRUE,
                      progress = TRUE,
                      mult_corr = FALSE,
                      poisson_test = FALSE,
                      env = parent.frame(),
                      event_names = c("ae")
) {

  call <- rlang::enexpr(df_visit)

  # save visit call
  visit <- tryCatch({
    visit <- orivisit(df_visit, call, env = env, event_names = event_names)
    as.data.frame(visit, env = env)
    visit},
    error = function(e) df_visit
  )

  # when two tbl objects passed automatically switch to inframe
  is_tbl_df_visit <- ! is.data.frame(df_visit) & inherits(df_visit, "tbl")
  is_tbl_r <- ! is.data.frame(r) & inherits(r, "tbl")

  if (is_tbl_df_visit && is_tbl_r) {
    inframe <- TRUE
  }

  for (x in event_names) {
    if (!(paste0("n_", x) %in% colnames(df_visit))) {
      stop(paste0(x, " not found in df_visit"))
      }
  }

  # multiple event_names only work for inframe method
  if (length(event_names) > 1) {
    inframe <- TRUE
  }

  # under only is only allowed for classic algo
  if (inframe) {
    under_only <- FALSE
  }

  # classic algorithm requires visit_med75
  if (! inframe) {
    visit_med75 <- TRUE
  }

  if (visit_med75 && ! inframe) {

    if (! event_names == "ae") {
      stop("Event_name must be 'ae' if inframe is FALSE")
    }

    aerep <- simaerep_visit_med75(
      df_visit = visit,
      under_only = under_only,
      r = r,
      progress = progress,
      poisson_test = poisson_test,
      env = env,
      check = check,
      mult_corr = mult_corr
    )

  } else {
    aerep <- simaerep_inframe(
      df_visit = visit,
      r = r,
      under_only = under_only,
      visit_med75 = visit_med75,
      env = env,
      check = check,
      event_names = event_names,
      mult_corr = mult_corr
    )
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
                              env = parent.frame(),
                              event_names = c("ae"),
                              mult_corr = FALSE
) {

  if (inherits(df_visit, "orivisit")) {
    visit <- df_visit
    df_visit <- as.data.frame(df_visit, env = env)
  } else {
    call <- rlang::enexpr(df_visit)
    visit <- orivisit(df_visit, call, env = env, event_names = event_names)
  }

  # check --------------------------------------------
  if (check) {
    df_visit <- check_df_visit(df_visit, event_names)
  }

  # aggregate ----------------------------------------
  if (visit_med75) {
    site_aggr_method <- "med75_adj"
  } else {
    site_aggr_method <- "max"
  }

  df_site <- site_aggr(
    df_visit,
    method = site_aggr_method,
    check = FALSE,
    event_names = event_names
  )

  # sim_inframe will truncate data to include only visit_med75 data before simulating
  if (visit_med75) {
    df_sim_sites <- sim_inframe(df_visit, r = r, df_site = df_site, event_names = event_names)
  } else {
    df_sim_sites <- sim_inframe(df_visit, r = r, event_names = event_names)
  }

  # evaluate ---------------------------------------

  if (mult_corr) {
    eval_sites_method <- "BH"
  } else {
    eval_sites_method <- NULL
  }

  df_eval <- eval_sites(
    df_sim_sites,
    method = eval_sites_method,
    under_only = under_only,
    visit_med75 = visit_med75
  )

  # df_sim_sites df does not include visit_med75 stats
  if (visit_med75) {
    df_eval <- df_eval %>%
      left_join(
        df_site %>%
          select(- "n_pat"),
        by = c("study_id", "site_number")
      ) %>%
      select(- paste0("mean_", event_names, "_site_med75"))
  }

  validate_simaerep(
    new_simaerep(
      visit = visit,
      df_site = df_site,
      df_sim_sites = df_sim_sites,
      df_eval = df_eval,
      r = r,
      visit_med75 = visit_med75,
      inframe = TRUE,
      under_only = under_only,
      event_names = event_names,
      mult_corr = mult_corr,
      poisson_test = FALSE
    )
  )
}

simaerep_visit_med75 <- function(df_visit,
                                 check = TRUE,
                                 progress = TRUE,
                                 env = parent.frame(),
                                 under_only = TRUE,
                                 r = 1000,
                                 mult_corr = FALSE,
                                 poisson_test = FALSE) {

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

  df_site <- site_aggr(
    df_visit,
    method = "med75_adj",
    check = FALSE
  )

  df_sim_sites <- sim_sites(
    df_site = df_site,
    df_visit = df_visit,
    r = r,
    poisson_test = poisson_test,
    under_only = under_only,
    check = FALSE
  )

  if (mult_corr) {
    eval_sites_method <- "BH"
  } else {
    eval_sites_method <- NULL
  }

  df_eval <- eval_sites(
    df_sim_sites,
    method = eval_sites_method,
    under_only = under_only,
    visit_med75 = TRUE
  )

  validate_simaerep(
    new_simaerep(
      visit = visit,
      df_site = df_site,
      df_sim_sites = df_sim_sites,
      df_eval = df_eval,
      r = r,
      visit_med75 = TRUE,
      inframe = FALSE,
      under_only = under_only,
      mult_corr = mult_corr,
      poisson_test = poisson_test
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
#' @param plot_event vector containing the events that should be plotted, default = "ae"
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
                          env = parent.frame(),
                          plot_event = "ae") {

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

  p <- .f(df_visit, x, study, n_sites, plot_event, ...)

  return(p)

}

plot_simaerep_plot_study <- function(df_visit, x, study, n_sites, plot_event = "ae", ...) {
  study_plot <- purrr::map((plot_event),
                           function(event) {
                             plot_study(
                               df_visit = df_visit,
                               df_site = x$df_site,
                               df_eval = x$df_eval,
                               study = study,
                               n_sites = n_sites,
                               event_names = x$event_names,
                               plot_event = event,
                               mult_corr = x$mult_corr,
                               ...
                            )
                           }
  )

  cowplot::plot_grid(plotlist = study_plot, nrow = length(plot_event))
}

plot_simaerep_plot_visit_med75 <- function(df_visit, x, study, n_sites, plot_event = "ae", ...) {
  med75_plot <- purrr::map((plot_event),
                           function(event) {
                               plot_visit_med75(
                               df_visit = df_visit,
                               study_id_str = study,
                               n_sites = n_sites,
                               event_names = x$event_names,
                               plot_event = event,
                               ...
                              )
                            }
                          )
  cowplot::plot_grid(plotlist = med75_plot, nrow = length(plot_event))
}

#' @export
print.simaerep <- function(x, ...) {
  cat(
    paste(
      c(
        "simaerep object:",
        "Check aerep$df_eval prob column for reporting probabililty.",
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
