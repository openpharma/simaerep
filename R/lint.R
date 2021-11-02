
#' @title wrapper around lintr::lint_packages
#' @description throws error when code issues are detected and defines default
#'   linting rules for this package.
#' @details lintr::line_length_linter(120), trailing_whitespace_linter = NULL,
#'   cyclocomp_linter = lintr::cyclocomp_linter(25)
#' @param path character
#' @inheritParams lintr::lint_package
#' @return " "
#' @export
#' @seealso \code{\link[lintr]{lint_package}}
#' @rdname lint_package
#' @export
lint_package <- function(path = ".", ...) {

  lint_results <- lintr::lint_package(path = path,
                                        linters = lintr::with_defaults(
                                          line_length_linter = lintr::line_length_linter(120),
                                          trailing_whitespace_linter = NULL,
                                          cyclocomp_linter = lintr::cyclocomp_linter(25)
                                        ),
                                      exclusions = list("inst/logo/logo.R", "tests/spelling.R"),
                                      ...)

  if (length(lint_results) > 0) {
    print(lint_results) # does not work wit cat()
    stop(paste(lint_results))
  } else {
    print("no lint violations")
  }
}
