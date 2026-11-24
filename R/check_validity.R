# Checks of validity against the specification: allowed values, ranges,
# patterns and dates in the future.

#' Checks of validity
#'
#' A range is read as numbers for numeric columns and as dates for date
#' columns. A multiple-choice column whose answers are separated by spaces
#' is checked answer by answer when the specification lists its allowed
#' values.
#'
#' @inheritParams structure_checks
#' @return A [check_result()].
#' @name validity_checks
#' @examples
#' spec <- spec_complete(tibble::tibble(column = "sex", type = "category",
#'                                      allowed = "female;male"))
#' check_allowed_values(data.frame(sex = c("female", "Male")), spec, qualitas_options())
NULL

#' @rdname validity_checks
#' @export
check_allowed_values <- function(data, spec, options) {
  listed <- dplyr::filter(spec_present(spec, data), !is.na(.data$allowed))
  found <- per_column(data, listed, outside_allowed)
  check_result(found, checked = sum(purrr::map_int(listed$column, \(c) sum(!is_blank(data[[c]])))))
}

# The values of one column that are not allowed: neither the whole value nor
# every space-separated answer of a multiple-choice value is in the list.
outside_allowed <- function(x, row) {
  allowed <- split_list(row$allowed)
  text <- trimws(as_text(x))
  answers <- strsplit(dplyr::coalesce(text, ""), "\\s+")
  every_answer <- purrr::map_lgl(answers, \(a) all(a %in% allowed))
  cell_issues(x, row$column, !is_blank(x) & !(text %in% allowed) & !every_answer,
              "the value is not an allowed value")
}

#' @rdname validity_checks
#' @export
check_range <- function(data, spec, options) {
  ranged <- dplyr::filter(spec_present(spec, data), !is.na(.data$min) | !is.na(.data$max))
  found <- per_column(data, ranged, range_breaks)
  check_result(found, checked = sum(purrr::map_int(ranged$column, \(c) sum(!is_blank(data[[c]])))))
}

# The values of one column below its minimum or above its maximum, compared
# as dates for date columns and as numbers otherwise.
range_breaks <- function(x, row) {
  dated <- row$type %in% c("date", "datetime")
  read <- if (dated) as.numeric(as_day(x)) else as_number(x)
  bound <- if (dated) \(b) as.numeric(as_day(b)) else as_number
  low <- dplyr::coalesce(bound(row$min), -Inf)
  high <- dplyr::coalesce(bound(row$max), Inf)
  said <- dplyr::if_else(read < low, paste("the value is below the minimum,", row$min),
                         paste("the value is above the maximum,", row$max))
  cell_issues(x, row$column, !is.na(read) & (read < low | read > high), said)
}

#' @rdname validity_checks
#' @export
check_pattern <- function(data, spec, options) {
  patterned <- dplyr::filter(spec_present(spec, data), !is.na(.data$pattern))
  found <- per_column(data, patterned, \(x, row) cell_issues(
    x, row$column, !is_blank(x) & !grepl(row$pattern, as_text(x), perl = TRUE),
    paste0("the value does not match the pattern ", row$pattern)))
  check_result(found,
               checked = sum(purrr::map_int(patterned$column, \(c) sum(!is_blank(data[[c]])))))
}

#' @rdname validity_checks
#' @export
check_future_dates <- function(data, spec, options) {
  dated <- dplyr::filter(spec_present(spec, data),
                         .data$type %in% c("date", "datetime") | .data$role == "date")
  found <- per_column(data, dated, \(x, row) cell_issues(
    x, row$column, as_day(x) > as.Date(options$as_of),
    paste("the date is after", format(as.Date(options$as_of)))))
  dates <- purrr::map_int(dated$column, \(c) sum(!is.na(as_day(data[[c]]))))
  check_result(found, checked = sum(dates))
}
