#' @title execute a purrr or furrr function with a progress bar
#' @description call still needs to be wrapped in with_progress()
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

  if(.progress){
    p <- progressr::progressor(steps = .steps)
  } else {
    p <- NULL
  }

  f <- function(..., .f_args, p = p) {
    if (.progress) p()
    if (.slow) Sys.sleep(0.25)
    .f_args <- c(list(...), .f_args)
    do.call(.f, .f_args)
  }

  do.call(
    .purrr,
    c(
      list(...),
      list(f, .f_args = .f_args, p = p),
      .purrr_args
      )
  )
}




