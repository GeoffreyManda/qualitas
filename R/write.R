#' Write the results of an assessment to files
#'
#' Writes the issues, measures and scores as CSV files, and the whole
#' assessment (with its specification and settings) as one JSON file that
#' other tools can read. A dashboard, a database or a second language can
#' take the results from these files.
#'
#' @param x An assessment from [assess()].
#' @param dir The folder to write into; it is created if needed.
#' @param prefix The start of each file name.
#' @return The paths written, invisibly.
#' @export
#' @examples
#' result <- assess(data.frame(id = c(1, 1), x = c("a", NA)), name = "toy")
#' write_results(result, tempfile())
write_results <- function(x, dir, prefix = "qualitas") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  paths <- file.path(dir, paste0(prefix, c("-issues.csv", "-measures.csv", "-scores.csv",
                                           ".json")))
  readr::write_csv(x$issues, paths[1], na = "")
  readr::write_csv(x$measures, paths[2], na = "")
  readr::write_csv(x$scores, paths[3], na = "")
  jsonlite::write_json(as_json_list(x), paths[4], auto_unbox = TRUE, pretty = TRUE,
                       digits = NA, na = "null", null = "null")
  invisible(paths)
}

# The assessment as a plain list, with the settings that are functions or
# tables left out.
as_json_list <- function(x) {
  settings <- purrr::keep(x$options, \(value) is.atomic(value) && !is.null(value))
  list(qualitas_version = as.character(utils::packageVersion("qualitas")),
       name = x$name, created = format(x$created, "%Y-%m-%dT%H:%M:%S%z"),
       records = x$n_rows, columns = x$n_columns, overall_score = overall_score(x),
       scores = x$scores, measures = x$measures, issues = x$issues,
       specification = x$spec,
       settings = purrr::map(settings, \(value) if (inherits(value, "Date")) format(value)
                             else value))
}
