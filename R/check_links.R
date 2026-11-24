# Checks of integrity and timeliness: keys that point to no record of
# another table, data that are no longer current, and periods without
# records.

#' Checks of integrity and timeliness
#'
#' `check_foreign_keys()` reads the links in `options$relations`, a table
#' with the columns `column` (in the data), `table` (a name in
#' `options$tables`) and `key` (the column of that table it must match), and
#' flags values with no match. [assess_tables()] sets both for a set of
#' linked tables. `check_currentness()` flags a date column whose latest date
#' is more than `max_age_days` before `as_of`. `check_period_gaps()` flags
#' each `period` (month, week or quarter) between the first and last date
#' that holds no record.
#'
#' @inheritParams structure_checks
#' @return A [check_result()].
#' @name link_checks
#' @examples
#' visits <- data.frame(child = c("a", "b", "z"))
#' children <- data.frame(id = c("a", "b"))
#' links <- data.frame(column = "child", table = "children", key = "id")
#' check_foreign_keys(visits, infer_spec(visits),
#'                    qualitas_options(tables = list(children = children), relations = links))
NULL

#' @rdname link_checks
#' @export
check_foreign_keys <- function(data, spec, options) {
  links <- tibble::as_tibble(options$relations %||% tibble::tibble(column = character()))
  links <- dplyr::filter(links, .data$column %in% names(data))
  found <- purrr::pmap(links, \(column, table, key, ...) {
    known <- as_text(options$tables[[table]][[key]])
    x <- data[[column]]
    cell_issues(x, column, !is_blank(x) & !(as_text(x) %in% known),
                paste0("the value matches no ", key, " in ", table))
  }) |>
    purrr::list_rbind() |>
    dplyr::bind_rows(no_issues())
  check_result(found, checked = sum(purrr::map_int(links$column, \(c) sum(!is_blank(data[[c]])))))
}

#' @rdname link_checks
#' @export
check_currentness <- function(data, spec, options) {
  if (is.na(options$max_age_days)) return(check_result())
  dated <- date_columns(spec, data)
  latest <- as.Date(purrr::map_dbl(dated, \(c) latest_day(data[[c]])), origin = "1970-01-01")
  age <- as.numeric(as.Date(options$as_of) - latest)
  stale <- is.finite(age) & age > options$max_age_days
  check_result(tibble::tibble(column = dated[stale], value = format(latest[stale]),
                              message = sprintf("the latest date is %.0f days before %s",
                                                age[stale], format(as.Date(options$as_of)))),
               checked = length(dated))
}

# The latest date of a column, as a day number; missing when it holds none.
latest_day <- function(x) {
  days <- as.numeric(as_day(x))
  if (all(is.na(days))) NA_real_ else max(days, na.rm = TRUE)
}

#' @rdname link_checks
#' @export
check_period_gaps <- function(data, spec, options) {
  if (is.na(options$period)) return(check_result())
  dated <- columns_with_role(spec, data, "date")
  found <- purrr::map(dated, \(column) period_gaps(data[[column]], column, options$period)) |>
    purrr::list_rbind() |>
    dplyr::bind_rows(no_issues())
  check_result(found, checked = length(dated))
}

# The periods between a column's first and last date that hold no record.
period_gaps <- function(x, column, period) {
  days <- as_day(x)
  days <- days[!is.na(days)]
  if (length(days) < 2) return(no_issues())
  counts <- table(cut(days, breaks = period))
  empty <- names(counts)[counts == 0]
  tibble::tibble(column = rep(column, length(empty)), value = empty,
                 message = rep(paste("no record falls in the", period, "starting on this date"),
                               length(empty)))
}

# The columns that hold dates, by their type or their role.
date_columns <- function(spec, data) {
  union(columns_typed(spec, data, c("date", "datetime")), columns_with_role(spec, data, "date"))
}
