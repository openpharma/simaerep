
new_simaerep <- function(visit,
                         df_site,
                         df_sim_sites,
                         df_eval,
                         param_site_aggr,
                         param_sim_sites,
                         param_eval_sites) {

  structure(
    list(
      visit = visit,
      df_site = df_site,
      df_sim_sites = df_sim_sites,
      df_eval = df_eval,
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
      "param_site_aggr",
      "param_sim_sites",
      "param_eval_sites"
    )
  )

  stopifnot(all(obj_names == obj_names_check))

  stopifnot(is_orivisit(x$visit))
  stopifnot(is.data.frame(x$df_site))
  stopifnot(is.data.frame(x$df_sim_sites))
  stopifnot(is.data.frame(x$df_eval))
  stopifnot(is.list(x$param_site_aggr))
  stopifnot(is.list(x$param_sim_sites))
  stopifnot(is.list(x$param_eval_sites))

  return(x)
}

#'@title create simaerep object
#'@description simulate AE under-reporting probabilities
#'@param df_visit data frame with columns: study_id, site_number, patnum, visit,
#'  n_ae
#'@param param_site_aggr list of parameters passed to [site_aggr()][site_aggr],
#'  Default: list(method = "med75_adj", min_pat_pool = 0.2)
#'@param param_sim_sites list of parameters passed to [sim_sites()][sim_sites],
#'  Default: list(r = 1000, poisson_test = FALSE, prob_lower = TRUE)
#'@param param_eval_sites list of parameters passed to
#'  [eval_sites()][eval_sites], Default: list(method = "BH")
#'@param progress logical, display progress bar, Default = TRUE
#'@param check logical, perform data check and attempt repair with
#'  [check_df_visit()][check_df_visit], computationally expensive on large data
#'  sets. Default: TRUE
#'@param env optional, provide environment of original visit data, Default:
#'  parent.frame()
#'@return simaerep object
#'@details executes [site_aggr()][site_aggr], [sim_sites()][sim_sites] and
#'  [eval_sites()][eval_sites] on original visit data and stores all
#'  intermediate results. Stores lazy reference to original visit data for
#'  facilitated plotting using generic plot(x).
#' @examples
#'
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
#' aerep
#'
#' str(aerep)
#'
#'@seealso [site_aggr()][site_aggr], [sim_sites()][sim_sites],
#'  [eval_sites()][eval_sites], [orivisit()][orivisit], [plot.simaerep()][plot.simaerep]
#'@rdname simaerep
#'@export
simaerep <- function(df_visit,
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
                     progress = TRUE,
                     check = TRUE,
                     env = parent.frame()) {

  call <- rlang::enexpr(df_visit)

  visit <- orivisit(df_visit, call, env = env)

  if (check) {
    df_visit <- check_df_visit(df_visit)
  }

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
    study <- unique(x$df_eval$study_id)[[1]]
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
        "Check aerep$df_eval prob_low_prob_ur column for AE under-reporting probabilites.",
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
