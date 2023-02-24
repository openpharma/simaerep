#' @title Execute a purrr or furrr function with a progress
#' bar.
#' @description Internal utility function.
#' @details Call still needs to be wrapped in \code{\link[progressr]{with_progress}}
#' or [with_progress_cnd()][with_progress_cnd()]
#' @param .purrr purrr or furrr function
#' @param ... iterable arguments passed to .purrr
#' @param .f function to be executed over iterables
#' @param .f_args list of arguments passed to .f, Default: list()
#' @param .purrr_args list of arguments passed to .purrr, Default: list()
#' @param .steps integer number of iterations
#' @param .slow logical slows down execution, Default: FALSE
#' @param .progress logical, show progress bar, Default: TRUE
#' @examples
#' # purrr::map
#' progressr::with_progress(
#'   purrr_bar(rep(0.25, 5), .purrr = purrr::map, .f = Sys.sleep, .steps = 5)
#' )
#'
#'\donttest{
#' # purrr::walk
#' progressr::with_progress(
#'  purrr_bar(rep(0.25, 5), .purrr = purrr::walk,.f = Sys.sleep, .steps = 5)
#' )
#'
#' # progress bar off
#' progressr::with_progress(
#'   purrr_bar(
#'     rep(0.25, 5), .purrr = purrr::walk,.f = Sys.sleep, .steps = 5, .progress = FALSE
#'   )
#' )
#'
#' # purrr::map2
#' progressr::with_progress(
#'   purrr_bar(
#'     rep(1, 5), rep(2, 5),
#'     .purrr = purrr::map2,
#'     .f = `+`,
#'     .steps = 5,
#'     .slow = TRUE
#'  )
#')
#'
#' # purrr::pmap
#' progressr::with_progress(
#'   purrr_bar(
#'     list(rep(1, 5), rep(2, 5)),
#'     .purrr = purrr::pmap,
#'     .f = `+`,
#'     .steps = 5,
#'     .slow = TRUE
#'  )
#')
#'
#' # define function within purr_bar() call
#' progressr::with_progress(
#'   purrr_bar(
#'     list(rep(1, 5), rep(2, 5)),
#'     .purrr = purrr::pmap,
#'     .f = function(x, y) {
#'       paste0(x, y)
#'     },
#'     .steps = 5,
#'     .slow = TRUE
#'  )
#')
#'
#' # with mutate
#' progressr::with_progress(
#'  tibble::tibble(x = rep(0.25, 5)) %>%
#'   dplyr::mutate(x = purrr_bar(x, .purrr = purrr::map, .f = Sys.sleep, .steps = 5))
#' )
#'}
#' @return result of function passed to .f
#' @rdname purrr_bar
#' @export
purrr_bar <- function(...,
                      .purrr,
                      .f,
                      .f_args = list(),
                      .purrr_args = list(),
                      .steps,
                      .slow = FALSE,
                      .progress = TRUE) {

  stopifnot("all .f_args list items must be named" = all(names(.f_args) != ""))
  stopifnot("all .purrr_args list items must be named" = all(names(.purrr_args) != ""))

  if (.progress) {
    p <- progressr::progressor(steps = .steps)
  } else {
    p <- NULL
  }

  f <- function(..., .f_args, .p = p) {
    if (.progress) p()
    if (.slow) Sys.sleep(0.25)
    .f_args <- c(list(...), .f_args)
    do.call(.f, .f_args)
  }

  do.call(
    .purrr,
    c(
      list(...),
      list(f, .f_args = .f_args, .p = p),
      .purrr_args
      )
  )
}


#'@title Conditional \code{\link[progressr]{with_progress}}.
#'@description Internal function. Use instead of
#'  \code{\link[progressr]{with_progress}} within custom functions with progress
#'  bars.
#'@param ex expression
#'@param progress logical, Default: TRUE
#'@details This wrapper adds a progress parameter to \code{\link[progressr]{with_progress}}
#' so that we can control the progress bar in the user facing functions. The progressbar
#' only shows in interactive mode.
#' @examples
#' if (interactive()) {
#'
#'  with_progress_cnd(
#'    purrr_bar(rep(0.25, 5), .purrr = purrr::map, .f = Sys.sleep, .steps = 5),
#'    progress = TRUE
#'  )
#'
#'  with_progress_cnd(
#'    purrr_bar(rep(0.25, 5), .purrr = purrr::map, .f = Sys.sleep, .steps = 5),
#'    progress = FALSE
#'  )
#'
#' # wrap a function with progress bar with another call with progress bar
#'
#' f1 <- function(x, progress = TRUE) {
#'   with_progress_cnd(
#'     purrr_bar(x, .purrr = purrr::walk, .f = Sys.sleep, .steps = length(x), .progress = progress),
#'     progress = progress
#'   )
#' }
#'
#' # inner progress bar blocks outer progress bar
#' progressr::with_progress(
#'   purrr_bar(
#'     rep(rep(1, 3),3), .purrr = purrr::walk, .f = f1, .steps = 3,
#'     .f_args = list(progress = TRUE)
#'   )
#' )
#'
#' # inner progress bar turned off
#' progressr::with_progress(
#'   purrr_bar(
#'     rep(list(rep(0.25, 3)), 5), .purrr = purrr::walk, .f = f1, .steps = 5,
#'     .f_args = list(progress = FALSE)
#'   )
#' )
#'}
#'@return No return value, called for side effects
#'@seealso \code{\link[progressr]{with_progress}}
#'@rdname with_progress_cnd
#'@export
with_progress_cnd <- function(ex, progress = TRUE) {
  if (progress) {
    progressr::with_progress(eval(ex))
  } else {
    eval(ex)
  }
}
