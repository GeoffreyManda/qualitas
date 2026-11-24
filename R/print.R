#' Print or summarise an assessment
#'
#' `print()` shows the size of the data, the overall score, the score of
#' each dimension and the checks that found the most problems. `summary()`
#' returns the same figures as a list. The overall score is the mean of the
#' dimension scores, so each dimension counts once however many checks it
#' holds.
#'
#' @param x,object An assessment from [assess()].
#' @param n How many checks to list.
#' @param ... Not used.
#' @return `print()` returns `x` invisibly; `summary()` a list with `overall`,
#'   `scores`, `top_checks` and the counts of issues by severity.
#' @export
#' @examples
#' result <- assess(data.frame(id = c(1, 1, 2), age = c(30, NA, 300)), name = "toy")
#' print(result)
#' summary(result)$overall
print.qualitas_assessment <- function(x, n = 10, ...) {
  digest <- summary(x, n = n)
  cli::cli_h1("Data quality of {x$name}")
  cli::cli_text("{x$n_rows} record{?s}, {x$n_columns} column{?s}; ",
                "{nrow(x$issues)} issue{?s} from {sum(x$measures$flagged > 0)} check{?s}.")
  cli::cli_text("Overall score: {format_share(digest$overall)}")
  cli::cli_h2("Scores by dimension")
  cli::cat_line(format_table(dplyr::mutate(digest$scores,
                                           score = format_share(.data$score),
                                           lowest = format_share(.data$lowest))))
  cli::cli_h2("Checks with the most problems")
  cli::cat_line(format_table(dplyr::select(digest$top_checks, "check_id", "title",
                                           "severity", "flagged", "checked")))
  failed <- dplyr::filter(x$measures, .data$status == "error")
  if (nrow(failed) > 0) cli::cli_alert_warning("Checks that could not run: {failed$check_id}")
  invisible(x)
}

#' @rdname print.qualitas_assessment
#' @export
summary.qualitas_assessment <- function(object, n = 10, ...) {
  list(overall = overall_score(object),
       scores = object$scores,
       top_checks = object$measures |>
         dplyr::filter(.data$flagged > 0) |>
         dplyr::arrange(dplyr::desc(severity_weights()[.data$severity]),
                        dplyr::desc(.data$flagged)) |>
         utils::head(n),
       by_severity = dplyr::count(object$issues, .data$severity))
}

#' The overall score of an assessment
#'
#' @param x An assessment from [assess()].
#' @return A number between 0 and 1, or `NA` when no check applied.
#' @export
#' @examples
#' overall_score(assess(data.frame(x = c(1, 2, NA)), name = "toy"))
overall_score <- function(x) {
  if (nrow(x$scores) == 0) return(NA_real_)
  mean(x$scores$score)
}

# A share written as a percentage with one decimal.
format_share <- function(share) dplyr::if_else(is.na(share), "-", sprintf("%.1f%%", 100 * share))

# A data frame as plain text lines, for the console.
format_table <- function(table) {
  if (nrow(table) == 0) return("  (none)")
  utils::capture.output(print(as.data.frame(table), row.names = FALSE))
}
