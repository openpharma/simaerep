
new_simaerep <- function(visit,
                         df_site,
                         df_sim_sites,
                         df_eval,
                         r,
                         visit_med75,
                         inframe,
                         under_only,
                         event_names,
                         mult_corr,
                         poisson_test,
                         col_names
                         ) {

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
      poisson_test = poisson_test,
      col_names = col_names
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
      "poisson_test",
      "col_names"
    )
  )

  stopifnot(all(obj_names == obj_names_check))

  stopifnot(is_orivisit(x$visit))
  stopifnot(is.data.frame(x$df_site) | inherits(x$df_site, "tbl"))
  stopifnot(is.data.frame(x$df_sim_sites) | inherits(x$df_sim_sites, "tbl"))
  stopifnot(is.data.frame(x$df_eval) | inherits(x$df_eval, "tbl"))

  stopifnot(x$colnames[names(x$colnames) %in% c("study_id", "site_id")] %in% colnames(x$df_eval))
  stopifnot(x$colnames[names(x$colnames) %in% c("study_id", "site_id")] %in% colnames(x$df_site))
  stopifnot(x$colnames[names(x$colnames) %in% c("study_id", "site_id")] %in% colnames(x$df_sim_sites))

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
#'  Default: FALSE
#'@param inframe Logical, only table operations to be used; does not require
#'  visit_med75. Compatible with dbplyr supported database backends. Default: TRUE
#'@param mult_corr Logical, multiplicity correction, Default: TRUE
#'@param progress Logical, display progress bar. Default: TRUE.
#'@param env Optional, provide environment of original visit data. Default:
#'  parent.frame().
#'@param under_only Logical, compute under-reporting probabilities only.
#'  Supersedes under_only parameter passed to [eval_sites()] and [sim_sites()].
#'  Default: FALSE
#'@param poisson_test logical, compute p-value with poisson test, only supported
#'by the classic algorithm using visit_med75. Default: FALSE
#'@param event_names vector, contains the event names, default = "event"
#'@param col_names named list, indicate study_id, site_id, patient_id and visit
#'column in df_visit input dataframe. Default: list(
#'study_id = "study_id",
#'site_id = "site_id",
#'patient_id = "patient_id",
#'visit = "visit"
#')
#'@return A simaerep object. Results are contained in the attached df_eval dataframe.
#'
#' | Column Name               | Description                                  | Type     |
#' |---------------------------|----------------------------------------------|----------|
#' | study_id                  | The study ID                                 | Character|
#' | site_id.                  | The site ID                                  | Character|
#' | (event)_count             | Site event count                             | Numeric  |
#' | (event)_per_visit_site    | Site Ratio of event count divided by visits  | Numeric  |
#' | visits                    | Site visit count                             | Numeric  |
#' | n_pat                     | Site patient count                           | Numeric  |
#' | (event)_per_visit_study   | Simulated study ratio                        | Numeric  |
#' | (event)_prob              | Site event ratio probability from -1 to 1    | Numeric  |
#' | (event)_delta             | Difference expected vs reported events       | Numeric  |
#'
#' @details Executes [site_aggr()], [sim_sites()], and [eval_sites()] on original
#'  visit data and stores all intermediate results. Stores lazy reference to
#'  original visit data for facilitated plotting using generic plot(x).
#' @examples
#' df_visit <- sim_test_data_study(
#'   n_pat = 100,
#'   n_sites = 5,
#'   ratio_out = 0.4,
#'   factor_event_rate = - 0.6
#' )
#'
#' evrep <- simaerep(df_visit)
#' evrep
#' str(evrep)
#'
#' # simaerep classic algorithm
#'
#' evrep <- simaerep(df_visit, inframe = FALSE, under_only = TRUE, mult_corr = TRUE)
#' evrep
#'
#' # multiple events
#'
#' df_visit_events_test <- sim_test_data_study(
#'   n_pat = 100,
#'   n_sites = 5,
#'   ratio_out = 0.4,
#'   factor_event_rate = - 0.6,
#'   event_per_visit_mean = c(0.5, 0.3),
#'   event_names = c("ae", "pd")
#' )
#'
#' evsrep <- simaerep(df_visit_events_test, inframe = TRUE, event_names = c("ae", "pd"))
#'
#' evsrep
#'
#' \donttest{
#'# Database example
#'con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
#'df_r <- tibble::tibble(rep = seq(1, 1000))
#'dplyr::copy_to(con, df_visit, "visit")
#'dplyr::copy_to(con, df_r, "r")
#'tbl_visit <- dplyr::tbl(con, "visit")
#'tbl_r <- dplyr::tbl(con, "r")
#'simaerep(tbl_visit, r = tbl_r)
#'DBI::dbDisconnect(con)
#' }
#'@export
#'@seealso [site_aggr][site_aggr], [sim_sites][sim_sites],
#'  [eval_sites][eval_sites], [orivisit][orivisit],
#'  [plot.simaerep][plot.simaerep], [print.simaerep][print.simaerep],
#'  [simaerep_inframe][simaerep_inframe]
#'@rdname simaerep
simaerep <- function(df_visit,
                      r = 1000,
                      check = TRUE,
                      under_only = FALSE,
                      visit_med75 = FALSE,
                      inframe = TRUE,
                      progress = TRUE,
                      mult_corr = TRUE,
                      poisson_test = FALSE,
                      env = parent.frame(),
                      event_names = c("event"),
                      col_names = list(
                       study_id = "study_id",
                       site_id = "site_id",
                       patient_id = "patient_id",
                       visit = "visit"
                     )
) {

  # catch call
  call <- rlang::enexpr(df_visit)

  # correct event_names ----------------------------------------------------
  # when only one event_name passed use data to determine event name
  if (length(event_names) == 1) {

    possible_event_names <- map_col_names(df_visit, col_names) %>%
      select(any_of("n"), starts_with("n_")) %>%
      colnames()

    if (length(possible_event_names) == 1) {
      event_names <- unlist(str_replace(possible_event_names, "^n_", ""))
    }
  }

  for (x in event_names) {
    if (!paste0("n_", x) %in% c(colnames(df_visit), names(col_names))) {
      stop(paste0("n_", x, " not found in df_visit"))
    }
  }

  # get S3 orivisit ------------------------------------------------------

  visit <- tryCatch({
    visit <- orivisit(df_visit, call, env = env, event_names = event_names, col_names = col_names)
    as.data.frame(visit, env = env)
    visit},
    error = function(e) df_visit
  )

  # correct method -------------------------------------------------------

  # when two tbl objects passed automatically switch to inframe
  is_tbl_df_visit <- ! is.data.frame(df_visit) & inherits(df_visit, "tbl")
  is_tbl_r <- ! is.data.frame(r) & inherits(r, "tbl")

  if (is_tbl_df_visit && is_tbl_r) {
    inframe <- TRUE
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

  if (poisson_test) {
    visit_med75 <- TRUE
    under_only <- TRUE
    inframe <- FALSE
  }

  if (visit_med75 && ! inframe) {

    aerep <- simaerep_classic(
      df_visit = visit,
      under_only = under_only,
      r = r,
      progress = progress,
      poisson_test = poisson_test,
      env = env,
      check = check,
      mult_corr = mult_corr,
      event_names = event_names,
      col_names = col_names
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
      mult_corr = mult_corr,
      col_names = col_names
    )
  }

  return(aerep)

}

#' @rdname simaerep
#' @export
simaerep_inframe <- function(df_visit,
                              r = 1000,
                              under_only = FALSE,
                              visit_med75 = FALSE,
                              check = TRUE,
                              env = parent.frame(),
                              event_names = c("event"),
                              mult_corr = FALSE,
                              col_names = list(
                               study_id = "study_id",
                               site_id = "site_id",
                               patient_id = "patient_id",
                               visit = "visit"
                              )
) {

  if (inherits(df_visit, "orivisit")) {
    visit <- df_visit

    df_visit <- as.data.frame(df_visit, env = env) %>%
      map_col_names(visit$col_names)

  } else {
    call <- rlang::enexpr(df_visit)
    visit <- orivisit(
      df_visit,
      call = call,
      env = env,
      event_names = event_names,
      col_names = col_names
    )

    df_visit <- df_visit %>%
      map_col_names(visit$col_names)
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
    visit_med75 = visit_med75,
    event_names = event_names
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
      df_site = remap_col_names(df_site, col_names),
      df_sim_sites = remap_col_names(df_sim_sites, col_names),
      df_eval = remap_col_names(df_eval, col_names),
      r = r,
      visit_med75 = visit_med75,
      inframe = TRUE,
      under_only = under_only,
      event_names = event_names,
      mult_corr = mult_corr,
      poisson_test = FALSE,
      col_names = col_names
    )
  )
}

#' @rdname simaerep
#' @export
simaerep_classic <- function(df_visit,
                                 check = TRUE,
                                 progress = TRUE,
                                 env = parent.frame(),
                                 under_only = TRUE,
                                 r = 1000,
                                 mult_corr = FALSE,
                                 poisson_test = FALSE,
                                 event_names = "event",
                                 col_names = list(
                                   study_id = "study_id",
                                   site_id = "site_id",
                                   patient_id = "patient_id",
                                   visit = "visit"
                                 )) {


  if (inherits(df_visit, "orivisit")) {
    visit <- df_visit
    df_visit <- as.data.frame(df_visit, env = env) %>%
      map_col_names(visit$col_names)
  } else {
    call <- rlang::enexpr(df_visit)
    visit <- orivisit(df_visit, call, env = env, col_names = col_names)
    df_visit <- df_visit %>%
      map_col_names(visit$col_names)
  }

  df_visit <- map_col_names(df_visit, col_names)

  # check
  if (check) {
    df_visit <- check_df_visit(df_visit, event_names = event_names)
  }

  # event_names correction --------------------------------------------

  stopifnot(
    "event_names must be length == 1 for classic (inframe = FALSE) algorithm" =
     length(event_names) == 1
  )

  df_visit <- df_visit %>%
    rename(n_ae = glue("n_{event_names}"))


  # classic -----------------------------------------------------------

  df_site <- site_aggr(
    df_visit,
    method = "med75_adj",
    event_names = "ae"
  )

  df_sim_sites <- sim_sites(
    df_site = df_site,
    df_visit = df_visit,
    r = r,
    poisson_test = poisson_test,
    under_only = under_only
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

  # revert column names ---------------------------------------------

  df_site <- df_site %>%
    dplyr::rename_with(~ stringr::str_replace(., "ae", event_names)) %>%
    remap_col_names(col_names)

  df_sim_sites <- df_sim_sites %>%
    dplyr::rename_with(~ stringr::str_replace(., "ae", event_names)) %>%
    remap_col_names(col_names)

  df_eval <- df_eval %>%
    dplyr::rename_with(~ stringr::str_replace(., "ae", event_names)) %>%
    remap_col_names(col_names)

  # construct simearep object ---------------------------------------

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
      poisson_test = poisson_test,
      event_names = event_names,
      col_names = col_names
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
#'   ratio_out = 0.4,
#'   factor_event_rate = - 0.6
#' )
#'
#' evrep <- simaerep(df_visit)
#'
#' plot(evrep, what = "prob", study = "A")
#' plot(evrep, what = "med75", study = "A")
#' }
#' @rdname plot.simaerep
#' @export
plot.simaerep <- function(x,
                          ...,
                          study = NULL,
                          what = "prob",
                          n_sites = 16,
                          df_visit = NULL,
                          env = parent.frame(),
                          plot_event = x$event_names[1]) {

  .f <- switch(what,
    "prob" = plot_simaerep_plot_study,
    "med75" = plot_simaerep_plot_visit_med75,
    stop('what must be either "prob" or med75')
  )


  if (is.null(df_visit)) {
    df_visit <- as.data.frame(x$visit, env = env) %>%
      map_col_names(x$col_names)
  } else {
    df_visit <- map_col_names(df_visit, col_names = x$col_names)
  }

  df_eval <- map_col_names(x$df_eval, col_names = x$col_names)
  df_site <- map_col_names(x$df_site, col_names = x$col_names)

  if (is.null(study)) {
    studies <- df_eval %>%
      distinct(.data$study_id) %>%
      pull(.data$study_id) %>%
      sort()

    study <- studies[1]

    message(paste0("study = NULL, defaulting to study:", study))
  }

  p <- .f(df_visit, x, study, n_sites, plot_event, df_eval = df_eval, df_site = df_site, ...)

  return(p)

}

plot_simaerep_plot_study <- function(df_visit,
                                     x,
                                     study,
                                     n_sites,
                                     plot_event,
                                     df_eval,
                                     df_site,
                                     ...) {
  study_plot <- purrr::map(
     plot_event,
     function(event) {
       plot_study(
         df_visit = df_visit,
         df_site = df_site,
         df_eval = df_eval,
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
  med75_plot <- purrr::map(
     plot_event,
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

#' Print method for simaerep objects
#'
#' @param x An object of class 'simaerep'
#' @param ... Additional arguments passed to print (not used)
#' @param n Number of rows to display from df_eval (default: 5)
#'
#' @export
print.simaerep <- function(x, ..., n = 10) {
  cat("simaerep object:\n")
  cat("----------------\n")
  cat("Plot results using plot() generic.\n")
  cat('Full results available in "df_eval".\n\n')

  cat("Summary:\n")

  if (inherits(x$df_eval, "data.frame")){
    cat(sprintf("Number of sites: %d\n", nrow(x$df_eval)))
    cat(sprintf("Number of studies: %d\n\n", n_distinct(map_col_names(x$df_eval, x$col_names)$study_id)))
  }

  if (x$inframe) {
    if (x$mult_corr) {
      cat('Multiplicity correction applied to "*_prob" columns.\n\n')
    }
  } else {
    cat('Classic algorithm used to calculate probabilities!!\n\n')
    if (x$under_only) {
      cat('Only under-reporting probability calculated !!!\n\n')
    }

    if (x$poisson_test & x$mult_corr) {
      cat('Multiplicity correction applied to prob and pval column.\n\n')
    } else if (x$mult_corr) {
      cat('Multiplicity correction applied to prob column.\n\n')
    }

  }


  if (length(x$event_names) > 1) {
    cat(
      paste(
        'Reporting probabilities calculated for:',
        paste(x$event_names, collapse = ", "),
        "\n\n"
      )
    )
  }

  cat("First", n, "rows of df_eval:\n")
  print(utils::head(x$df_eval, n))

  invisible(x)
}

#' @title is simaerep class
#' @description internal function
#' @param x object
#' @return logical
#' @rdname is_simaerep
#' @keywords internal
is_simaerep <- function(x) {
  "simaerep" %in% class(x)
}



#'@keywords internal
map_col_names <- function(df_visit,
                          col_names) {

  stopifnot(all(c("study_id", "site_id", "patient_id", "visit") %in% names(col_names)))

  rename_cols <- c(
    study_id = col_names[["study_id"]],
    site_number = col_names[["site_id"]],
    patnum = col_names[["patient_id"]],
    visit = col_names[["visit"]],
    unlist(
      col_names[! names(col_names) %in% c("study_id", "site_id", "patient_id", "visit")]
    )
  )

  df_visit <- df_visit %>%
    rename(any_of(rename_cols))

  return(df_visit)
}

#' renames internal simaerep col_names to externally applied colnames
#'@keywords internal
remap_col_names <- function(df,col_names) {

  internal_col_names <- c(
    study_id = "study_id",
    site_id = "site_number",
    patient_id = "patnum",
    visit = "visit"
  )

  rename_cols <- setNames(internal_col_names, col_names[names(internal_col_names)])

  stopifnot(all(names(internal_col_names) %in% names(col_names)))

  df <- df %>%
    rename(any_of(rename_cols))

  return(df)
}

