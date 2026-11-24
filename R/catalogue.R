#' The catalogue of checks
#'
#' Every check qualitas runs, one row each: its identifier, the function that
#' runs it, what it looks for, the dimension it belongs to, the matching terms
#' of ISO/IEC 25012, of Kahn et al. (2016) and of the dataquieR framework, the
#' unit it counts (cell, record, column or table), its default severity and
#' the source of the method.
#'
#' @return A tibble with one row per check.
#' @export
#' @examples
#' checks_catalogue()
checks_catalogue <- function() {
  path <- system.file("catalogue", "checks.csv", package = "qualitas", mustWork = TRUE)
  readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()),
                  na = character(), progress = FALSE)
}

#' The result of one check
#'
#' Checks return the problems they found and the number of units they
#' examined, so that every measure can be stated as a share of what was
#' checked.
#'
#' @param issues A table with `column`, `row`, `value` and `message`; columns
#'   that are absent are added as missing.
#' @param checked The number of units (cells, records, columns or tables)
#'   the check examined; zero when the check did not apply.
#' @return A list with `issues` and `checked`.
#' @export
#' @examples
#' check_result(data.frame(column = "age", row = 3, value = "-4",
#'                         message = "below 0"), checked = 10)
check_result <- function(issues = no_issues(), checked = 0L) {
  list(issues = tidy_issues(issues), checked = as.integer(checked))
}

# An empty table of issues, with the four columns every check reports.
no_issues <- function() {
  tibble::tibble(column = character(), row = integer(), value = character(),
                 message = character())
}

# The issues of a check in the standard form: the four columns, in order, as
# text and whole numbers, with any column the check did not fill set to missing.
tidy_issues <- function(issues) {
  filled <- dplyr::bind_rows(no_issues(), tibble::as_tibble(issues) |>
                               dplyr::mutate(dplyr::across(dplyr::any_of(
                                 c("column", "value", "message")), as.character),
                                 dplyr::across(dplyr::any_of("row"), as.integer)))
  dplyr::select(filled, "column", "row", "value", "message")
}

# The weight of each severity when check scores are combined: a critical
# problem counts four times a minor one.
severity_weights <- function() c(critical = 4, major = 2, minor = 1)
