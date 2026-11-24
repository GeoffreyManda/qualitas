#' Assess a set of linked tables
#'
#' Assesses each table of a relational data set, such as households and
#' their members, or facilities and their monthly reports, and checks the
#' keys that link them (check IN01).
#'
#' @param tables A named list of data frames.
#' @param specs A named list of specifications, or paths to them; a table
#'   without one has its specification inferred.
#' @param relations A data frame of links, one row each, with the columns
#'   `from` (the table holding the key), `column` (the key), `table` (the
#'   table it refers to) and `key` (the column it must match there).
#' @param options Settings from [qualitas_options()].
#' @param ... Passed to [assess()], for example `checks`.
#' @return A named list of assessments, of class `qualitas_assessments`.
#' @export
#' @examples
#' households <- data.frame(hh_id = c("h1", "h2"), village = c("a", "b"))
#' members <- data.frame(member_id = 1:3, hh_id = c("h1", "h2", "h9"))
#' links <- data.frame(from = "members", column = "hh_id",
#'                     table = "households", key = "hh_id")
#' results <- assess_tables(list(households = households, members = members), relations = links)
#' issues(results$members, dimension = "integrity")
assess_tables <- function(tables, specs = list(), relations = NULL, options = qualitas_options(),
                          ...) {
  if (is.null(names(tables)) || any(!nzchar(names(tables)))) {
    cli::cli_abort("{.arg tables} must be a list in which every table has a name.")
  }
  links <- tibble::as_tibble(relations %||% tibble::tibble(from = character(),
                                                           column = character(),
                                                           table = character(),
                                                           key = character()))
  results <- purrr::imap(tables, \(data, name) {
    own <- utils::modifyList(options, list(tables = tables))
    own$relations <- dplyr::filter(links, .data$from == name)
    assess(data, spec = specs[[name]], options = own, name = name, ...)
  })
  structure(results, class = "qualitas_assessments")
}

#' @export
print.qualitas_assessments <- function(x, ...) {
  overview <- tibble::tibble(table = names(x),
                             records = purrr::map_int(x, "n_rows"),
                             issues = purrr::map_int(x, \(one) nrow(one$issues)),
                             overall = format_share(purrr::map_dbl(x, overall_score)))
  cli::cli_h1("Data quality of {length(x)} linked table{?s}")
  cli::cat_line(format_table(overview))
  invisible(x)
}
