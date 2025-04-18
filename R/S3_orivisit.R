

new_orivisit <- function(dim, df_summary, str_call, event_names = "ae") {
  structure(
    list(
      dim = dim,
      df_summary = df_summary,
      str_call = str_call,
      event_names = event_names
    ),
    class = "orivisit"
  )
}

validate_orivisit <- function(x) {
  comp <- sort(attributes(x)$names) ==  sort(c("dim", "df_summary", "str_call", "event_names"))
  stopifnot(all(comp))
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
#' @param df_visit dataframe with original visit data
#' @param call optional, provide call, Default: NULL
#' @param env optional, provide environment of original visit data, Default: parent.frame()
#' @param event_names vector, contains the event names, default = "ae"
#' @return orivisit object
#' @details Saves variable name of original visit data, checks whether it can be
#'   retrieved from parent environment and stores summary. Original data can be
#'   retrieved using as.data.frame(x).
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
#' visit <- orivisit(df_visit)
#'
#' object.size(df_visit)
#' object.size(visit)
#'
#' as.data.frame(visit)
#'
#' @rdname orivisit
#' @export
orivisit <- function(df_visit, call = NULL, env = parent.frame(), event_names = c("ae")) {


  if (is.null(call)) {
    call <- rlang::enexpr(df_visit)
  }
  stopifnot(inherits(df_visit, "data.frame") | inherits(df_visit, "tbl"))

  dim <- dim(df_visit)
  df_summary <- summarise_df_visit(df_visit, event_names = event_names)
  str_call <- get_str_var(call, env)


  validate_orivisit(
    new_orivisit(
      dim,
      df_summary,
      str_call,
      event_names
    )
  )
}

#' @export
as.data.frame.orivisit <- function(x, ..., env = parent.frame()) {
  if (is.na(x$str_call)) stop.orivisit()
  if (! exists(x$str_call, envir = env)) stop.orivisit()

  df <- rlang::env_get(env, x$str_call, inherit = TRUE)

  df_is_tbl <- ! inherits(df, "data.frame") & inherits(df, "tbl")

  ifelse(is.null(x$event_names), event_names <- "ae", event_names <- x$event_names)

  if (! df_is_tbl) {
    dim <- dim(df)
    df_summary <- summarise_df_visit(df, event_names)

    #all.equal produces either TRUE or a character string (instead of FALSE)
    if (is.character(all.equal(df_summary, x$df_summary, tolerance = 1e-4))) stop.orivisit()
    if (! all(dim == x$dim)) stop.orivisit()
  }
  return(df)
}

stop.orivisit <- function(...) {
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

#' @export
print.orivisit <- function(x, ...) {
  cat(
    paste(
      c(
        "orivisit object:",
        "Stores lazy reference to original visit data, use as.data.frame() to retrieve."
      ),
      collapse = "\n"
    )
  )

}

#' @title is orivisit class
#' @description internal function
#' @param x object
#' @rdname is_orivisit
#' @return logical
#' @export
is_orivisit <- function(x) {
  "orivisit" %in% class(x)
}
