#' Write a report of an assessment
#'
#' Writes a self-contained HTML page or, with the officer and flextable
#' packages installed, a Word document. The report gives the scores by
#' dimension, every check with the units it examined and flagged, the issues
#' found (up to `max_issues`), the specification assessed against, and a note
#' on how the measures and scores are computed.
#'
#' @param x An assessment from [assess()].
#' @param file The file to write: a name ending in `.html` or `.docx`.
#' @param max_issues The most issues to list; all are counted.
#' @return The path written, invisibly.
#' @export
#' @examples
#' result <- assess(data.frame(id = c(1, 1), age = c(30, 300)), name = "toy")
#' report(result, tempfile(fileext = ".html"))
report <- function(x, file, max_issues = 1000) {
  kind <- tolower(tools::file_ext(file))
  if (!kind %in% c("html", "docx")) cli::cli_abort("{.arg file} must end in .html or .docx.")
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  sections <- report_sections(x, max_issues)
  if (kind == "docx") report_docx(sections, file) else report_html(sections, file)
  cli::cli_inform("Report written to {.file {file}}.")
  invisible(file)
}

# The parts of a report, the same for every format: a title, an opening
# paragraph and a list of headed tables, each with an optional note.
report_sections <- function(x, max_issues) {
  shown <- utils::head(x$issues, max_issues)
  list(
    title = paste("Data quality of", x$name),
    opening = sprintf(paste("%s records and %s columns, assessed on %s against %s checks.",
                            "Overall score %s; %s issues found by %s checks."),
                      x$n_rows, x$n_columns, format(as.Date(x$options$as_of)),
                      nrow(x$measures), format_share(overall_score(x)), nrow(x$issues),
                      sum(x$measures$flagged > 0)),
    tables = list(
      list(heading = "Scores by dimension",
           table = dplyr::mutate(x$scores, dplyr::across(c("score", "lowest"), format_share)),
           note = paste("A dimension's score is the mean of its checks' scores, weighted 4,",
                        "2 and 1 for critical, major and minor checks; the lowest check score",
                        "is shown beside it.")),
      list(heading = "Checks",
           table = x$measures |>
             dplyr::mutate(score = format_share(.data$score)) |>
             dplyr::select("check_id", "title", "dimension", "severity", "unit", "checked",
                           "flagged", "score", "status"),
           note = paste("Each check counts the units it examined and the units it flagged;",
                        "its score is 1 - flagged / checked (Pipino, Lee and Wang 2002).")),
      list(heading = "Issues",
           table = dplyr::select(shown, "check_id", "severity", "column", "row", "value",
                                 "message"),
           note = if (nrow(x$issues) > max_issues) {
             sprintf("The first %s of %s issues are listed.", max_issues, nrow(x$issues))
           }),
      list(heading = "Specification",
           table = dplyr::select(x$spec, "column", "type", "role", "required", "unique",
                                 "min", "max", "allowed"),
           note = NULL)
    ),
    method = paste("Checks are grouped by the dimensions of ISO/IEC 25012 and mapped to the",
                   "harmonised framework of Kahn et al. (2016) and to dataquieR (Schmidt et",
                   "al. 2021); the mapping of each check is in the package catalogue,",
                   "checks_catalogue().")
  )
}
