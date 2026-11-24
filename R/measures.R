# One measure per check, in the form of Pipino, Lee and Wang (2002): the units
# the check examined, the units it flagged, and the share that passed
# (1 - flagged / checked). A check that examined nothing does not apply; a
# check that failed to run, in whole or in part, reports its error and has no
# score.
measure_checks <- function(catalogue, issues, results) {
  flagged <- issues |>
    dplyr::left_join(dplyr::select(catalogue, "check_id", "unit"), by = "check_id",
                     relationship = "many-to-one", na_matches = "never") |>
    dplyr::summarise(flagged = flagged_units(.data$unit[1], .data$column, .data$row,
                                             .data$value),
                     .by = "check_id")
  catalogue |>
    dplyr::mutate(checked = purrr::map_int(results, "checked"),
                  error = purrr::map_chr(results, "error")) |>
    dplyr::left_join(flagged, by = "check_id", relationship = "one-to-one",
                     na_matches = "never") |>
    dplyr::mutate(
      flagged = dplyr::coalesce(.data$flagged, 0L),
      flagged = pmin(.data$flagged, .data$checked),
      score = dplyr::if_else(.data$checked > 0 & is.na(.data$error),
                             1 - .data$flagged / .data$checked, NA_real_),
      status = dplyr::case_when(!is.na(.data$error) ~ "error",
                                .data$checked == 0 ~ "not applicable",
                                .data$flagged == 0 ~ "passed",
                                .default = "issues found")
    ) |>
    dplyr::select("check_id", "title", "dimension", "severity", "unit", "checked",
                  "flagged", "score", "status", "error", "iso25012", "kahn",
                  "dataquier", "source")
}

# The number of distinct units flagged: cells are column-row pairs, records
# are rows, columns are columns, groups are column-value pairs (an
# enumerator and a measure), and a table counts once.
flagged_units <- function(unit, column, row, value) {
  units <- switch(unit, cell = dplyr::n_distinct(paste(column, row)),
                  record = dplyr::n_distinct(row), column = dplyr::n_distinct(column),
                  group = dplyr::n_distinct(paste(column, value)), 1L)
  as.integer(units)
}

# The score of each dimension: the mean of its checks' scores weighted by
# severity, over the checks that applied; the lowest check score beside it.
score_dimensions <- function(measures) {
  measures |>
    dplyr::filter(!is.na(.data$score)) |>
    dplyr::mutate(weight = unname(severity_weights()[.data$severity])) |>
    dplyr::summarise(checks = dplyr::n(),
                     checks_with_issues = sum(.data$flagged > 0),
                     lowest = if (length(.data$score)) min(.data$score) else NA_real_,
                     score = stats::weighted.mean(.data$score, .data$weight),
                     .by = "dimension") |>
    dplyr::relocate("lowest", .after = "score") |>
    dplyr::arrange(.data$score)
}

#' Read the results of an assessment
#'
#' * `issues()`: every problem found, one row each, with the check, its
#'   dimension and severity, the column, the row, the value and a message.
#' * `measures()`: one row per check, with the units checked and flagged, the
#'   share that passed, its status and its mapping to ISO/IEC 25012, Kahn et
#'   al. (2016) and dataquieR.
#' * `scores()`: one row per dimension, with the severity-weighted mean of its
#'   check scores and the lowest of them.
#'
#' @param x An assessment from [assess()].
#' @param dimension,severity Keep only issues of these dimensions or
#'   severities.
#' @return A tibble.
#' @export
#' @examples
#' result <- assess(data.frame(id = c(1, 1, 2), age = c(30, NA, 300)), name = "toy")
#' issues(result)
#' measures(result)
#' scores(result)
issues <- function(x, dimension = NULL, severity = NULL) {
  dplyr::filter(x$issues, is.null(dimension) | .data$dimension %in% dimension,
                is.null(severity) | .data$severity %in% severity)
}

#' @rdname issues
#' @export
measures <- function(x) x$measures

#' @rdname issues
#' @export
scores <- function(x) x$scores
