
#' @title wrapper around lintr::lint_packages which throws error when code
#'   issues are detected and defines default linting rules for this package.
#' @description lintr::line_length_linter(120), trailing_whitespace_linter =
#'   NULL, cyclocomp_linter = lintr::cyclocomp_linter(25)
#' @param path character
#' @inheritParams lintr::lint_package
#' @return " "
#' @details " "
#' @export
#' @seealso \code{\link[lintr]{lint_package}}
#' @rdname lint_package
#' @export
#' @importFrom lintr lint_package
lint_package <- function(path = ".", ...) {

  lint_results <- lintr::lint_package(path = path,
                                        linters = lintr::with_defaults(
                                          line_length_linter = lintr::line_length_linter(120),
                                          trailing_whitespace_linter = NULL,
                                          cyclocomp_linter = lintr::cyclocomp_linter(25)
                                        ),
                                      ...)

  if (length(lint_results) > 0) {
    print(lint_results) # does not work wit cat()
    stop(paste(lint_results))
  } else {
    print("no lint violations")
  }
}
