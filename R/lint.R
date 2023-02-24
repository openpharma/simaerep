
#' @title Wrapper for \code{\link[lintr]{lint_package}}.
#' @description Throws error when code issues are detected and defines default
#'   linting rules for this package.
#' @details lintr::line_length_linter(120), trailing_whitespace_linter = NULL,
#'   cyclocomp_linter = lintr::cyclocomp_linter(25)
#' @param path character
#' @inheritParams lintr::lint_package
#' @return No return value, called for side effects
#' @seealso \code{\link[lintr]{lint_package}}
#' @rdname lint_package
#' @noRd
lint_package <- function(path = ".", ...) {

  lint_results <- lintr::lint_package(path = path,
                                        linters = lintr::linters_with_defaults(
                                          line_length_linter = lintr::line_length_linter(120),
                                          trailing_whitespace_linter = NULL,
                                          cyclocomp_linter = lintr::cyclocomp_linter(25)
                                        ),
                                      exclusions = list("inst/logo/logo.R", "tests/spelling.R", "vignettes"),
                                      ...)

  if (length(lint_results) > 0) {
    message(lint_results) # does not work wit cat()
    stop(paste(lint_results))
  } else {
    message("no lint violations")
  }
}
