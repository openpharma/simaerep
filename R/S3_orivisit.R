

new_orivisit <- function(dim, df_summary, str_call, event_names, col_names) {
  structure(
    list(
      dim = dim,
      df_summary = df_summary,
      str_call = str_call,
      event_names = event_names,
      col_names = col_names
    ),
    class = "orivisit"
  )
}

validate_orivisit <- function(x) {

  expected_attributes <- c(
    "dim",
    "df_summary",
    "str_call",
    "event_names",
    "col_names"
  )

  stopifnot(all(sort(attributes(x)$names) ==  sort(expected_attributes)))
  stopifnot(length(x$dim) == 2)
  inherits(x$df_summary, "data.frame") | inherits(x$df_summary, "tbl")
  stopifnot(is.character(x$str_call) | is.na(x$str_call))
  return(x)
}


summarise_df_visit <- function(df_visit, event_names = c("ae")) {
  colsearch <- paste0("n_", event_names)
  colnames <- paste0("n_", event_names, "s") #nolint

  df_visit %>%
    group_by(
      .data$study_id,
      .data$site_number,
      .data$patnum
    ) %>%
    filter(visit == max(.data$visit)) %>%
    ungroup() %>%
    summarise(
      n_studies = n_distinct(.data$study_id),
      n_sites = n_distinct(.data$site_number),
      n_patients = n_distinct(.data$patnum),
      n_visits = sum(.data$visit),
      across(all_of({
        colsearch}), sum, .names = "{colnames}")
    )
}

get_str_var <- function(call, env) {
  str_call <- deparse(call)
  if (sum(str_length(str_call)) > 80) {

    return(NA)
  }

  # exists() will search all envs up to global env
  # env_has() will check custom env
  if (! exists(str_call) && ! rlang::env_has(env, str_call)) {

    return(NA)
  }
  return(str_call)
}

#' @title create orivisit object
#' @description Internal S3 object, stores lazy reference to original visit
#'   data.
#' @param call optional, provide call, Default: NULL
#' @inheritParams simaerep
#' @return orivisit object
#' @details Saves variable name of original visit data, checks whether it can be
#'   retrieved from parent environment and stores summary. Original data can be
#'   retrieved using as.data.frame(x).
#' @examples
#'
#' df_visit <- sim_test_data_study(
#'   n_pat = 100,
#'   n_sites = 5,
#'   ratio_out = 0.4,
#'   factor_event_rate = - 0.6
#' )#'
#'
#' visit <- orivisit(df_visit)
#'
#' object.size(df_visit)
#' object.size(visit)
#'
#' as.data.frame(visit)
#'
#' @rdname orivisit
#' @export
orivisit <- function(df_visit,
                     call = NULL,
                     env = parent.frame(),
                     event_names = c("event"),
                     col_names = list(
                         study_id = "study_id",
                         site_id = "site_id",
                         patient_id = "patient_id",
                         visit = "visit"
                       )
                     ) {


  if (is.null(call)) {
    call <- rlang::enexpr(df_visit)
  }
  stopifnot(inherits(df_visit, "data.frame") | inherits(df_visit, "tbl"))

  df_visit <- map_col_names(df_visit, col_names)

  dim <- dim(df_visit)
  df_summary <- summarise_df_visit(df_visit, event_names = event_names)
  str_call <- get_str_var(call, env)

  validate_orivisit(
    new_orivisit(
      dim,
      df_summary,
      str_call,
      event_names,
      col_names
    )
  )
}

#' @export
as.data.frame.orivisit <- function(x, ..., env = parent.frame()) {
  if (is.na(x$str_call)) error(x)
  if (! exists(x$str_call, envir = env)) error(x)

  df <- rlang::env_get(env, x$str_call, inherit = TRUE)

  df_is_tbl <- ! inherits(df, "data.frame") & inherits(df, "tbl")

  if (! df_is_tbl) {

    df_check <- df %>%
      map_col_names(col_names = x$col_names)

    dim <- dim(df_check)
    df_summary <- summarise_df_visit(df_check, x$event_names)

    #all.equal produces either TRUE or a character string (instead of FALSE)
    if (is.character(all.equal(df_summary, x$df_summary, tolerance = 1e-4))) {
      error(x) # covr mistake catch with browser()
    }

    if (! all(dim == x$dim)){
      error(x) # covr mistake catch with browser()
    }
  }

  return(df)
}

#' @keywords internal
error <- function(x, ...) {
  UseMethod("error")
}

#' @keywords internal
error.orivisit <- function(x, ...) {
  err <- structure(
    list(
      message = paste(
        "Could not find original visit data in parent environment.",
        "Please pass original visit data to function call."
      ),
      call = call,
      ...
    ),
    class = c("stop.orivisit", "error", "condition")
  )
  stop(err)
}



#' Print method for orivisit objects
#'
#' @param x An object of class 'orivisit'
#' @param ... Additional arguments passed to print (not used)
#' @param n Number of rows to display from the data frame (default: 10)
#' @keywords internal
#' @export
print.orivisit <- function(x, ..., n = 10) {
  cat("orivisit object:\n")
  cat("----------------\n")
  cat("Stores lazy reference to original visit data.\n")
  cat('Full data available via as.data.frame(x).\n\n')

  cat("Summary:\n")

  if (!is.null(x$df_summary) && inherits(x$df_summary, "data.frame")) {
    cat(sprintf("Number of studies: %d\n", x$df_summary$n_studies))
    cat(sprintf("Number of sites: %d\n", x$df_summary$n_sites))
    cat(sprintf("Number of patients: %d\n", x$df_summary$n_patients))
    cat(sprintf("Number of visits: %d\n", x$df_summary$n_visits))

    for (event in x$event_names) {
      cat(sprintf("Number of %s : %d\n", event, x$df_summary[[paste0("n_", event, "s")]]))
    }

  }

  cat("\n")

  cat(sprintf("Data dimensions: %d rows x %d columns\n", x$dim[1], x$dim[2]))
  cat(sprintf("Data source: %s\n", x$str_call))

  if (length(x$event_names) > 1) {
    cat(
      paste(
        'Event types:',
        paste(x$event_names, collapse = ", "),
        "\n"
      )
    )
  } else {
    cat(sprintf("Event type: %s\n", x$event_names))
  }

  cat("\nColumn mappings:\n")
  cat(sprintf("  study_id: %s\n", x$col_names$study_id))
  cat(sprintf("  site_id: %s\n", x$col_names$site_id))
  cat(sprintf("  patient_id: %s\n", x$col_names$patient_id))
  cat(sprintf("  visit: %s\n", x$col_names$visit))


  invisible(x)
}


#' @title is orivisit class
#' @description internal function
#' @param x object
#' @rdname is_orivisit
#' @return logical
#' @keywords internal
is_orivisit <- function(x) {
  "orivisit" %in% class(x)
}
