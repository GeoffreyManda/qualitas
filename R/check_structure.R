# Checks of the structure of the data: columns that are empty, constant,
# badly named, missing from the data or missing from the specification, and
# values that do not have their declared type.

#' Checks of structure and type
#'
#' Each check takes the data, its specification and the settings, and
#' returns a [check_result()]. They are run by [assess()]; they are exported
#' so that they can be run, tested and reused one at a time.
#'
#' @param data A data frame.
#' @param spec A specification (see [spec_template()]).
#' @param options Settings from [qualitas_options()].
#' @return A [check_result()].
#' @name structure_checks
#' @examples
#' data <- data.frame(a = c(NA, NA), b = c(1, 1), `a` = 1:2, check.names = FALSE)
#' check_empty_columns(data, infer_spec(data), qualitas_options())
NULL

#' @rdname structure_checks
#' @export
check_empty_columns <- function(data, spec, options) {
  empty <- names(data)[purrr::map_lgl(data, \(x) all(is_blank(x)))]
  check_result(tibble::tibble(column = empty, message = "the column holds no values"),
               checked = ncol(data))
}

#' @rdname structure_checks
#' @export
check_constant_columns <- function(data, spec, options) {
  values <- purrr::map(data, \(x) unique(as_text(x)[!is_blank(x)]))
  filled <- purrr::map_int(data, \(x) sum(!is_blank(x)))
  constant <- names(data)[lengths(values) == 1 & filled > 1]
  check_result(tibble::tibble(column = constant,
                              value = purrr::map_chr(values[constant], 1),
                              message = "every record holds the same value"),
               checked = ncol(data))
}

#' @rdname structure_checks
#' @export
check_column_names <- function(data, spec, options) {
  name <- names(data)
  problem <- dplyr::case_when(!nzchar(name) | is.na(name) ~ "the column has no name",
                              duplicated(name) ~ "the name repeats an earlier column",
                              name != make.names(name) ~ "the name is not a syntactic R name",
                              .default = NA_character_)
  check_result(tibble::tibble(column = name, message = problem) |>
                 dplyr::filter(!is.na(.data$message)),
               checked = length(name))
}

#' @rdname structure_checks
#' @export
check_declared_columns <- function(data, spec, options) {
  absent <- setdiff(spec$column, names(data))
  undeclared <- setdiff(names(data), spec$column)
  check_result(tibble::tibble(column = c(absent, undeclared),
                              message = c(rep("declared in the specification but absent",
                                              length(absent)),
                                          rep("present but not declared in the specification",
                                              length(undeclared)))),
               checked = length(union(spec$column, names(data))))
}

#' @rdname structure_checks
#' @export
check_type_conformance <- function(data, spec, options) {
  typed <- dplyr::filter(spec_present(spec, data),
                         .data$type %in% c("integer", "number", "date", "datetime", "logical"))
  found <- per_column(data, typed, type_breaks)
  filled <- sum(purrr::map_int(typed$column, \(column) sum(!is_blank(data[[column]]))))
  check_result(found, checked = filled)
}

# The values of one column that are present but do not read as its
# declared type.
type_breaks <- function(x, row) {
  present <- !is_blank(x)
  text <- trimws(as_text(x))
  readable <- switch(row$type,
                     integer = !is.na(as_number(text)) & as_number(text) %% 1 == 0,
                     number = !is.na(as_number(text)),
                     date = !is.na(as_day(text)),
                     datetime = !is.na(as_day(text)),
                     logical = toupper(text) %in% c("TRUE", "FALSE", "T", "F", "1", "0",
                                                    "YES", "NO"))
  cell_issues(x, row$column, present & !readable,
              paste0("the value is not of the declared type, ", row$type))
}
